/** Compute the matrix inverse via Gauss-Jordan elimination.
 *  This program uses only barriers to separate computation steps but no
 *  mutexes. It is an example of a race-free program on which no data races
 *  are reported by the happens-before algorithm (drd), but a lot of data races
 *  (all false positives) are reported by the Eraser-algorithm (helgrind).
 */


#define _GNU_SOURCE

/***********************/
/* Include directives. */
/***********************/

#include <assert.h>
#include <math.h>
#include <limits.h>  // PTHREAD_STACK_MIN
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>  // getopt()


/*********************/
/* Type definitions. */
/*********************/

typedef double elem_t;

struct gj_threadinfo
{
  pthread_barrier_t* b;
  pthread_t tid;
  elem_t* a;
  int rows;
  int cols;
  int r0;
  int r1;
};


/********************/
/* Local variables. */
/********************/

static int s_nthread = 1;


/*************************/
/* Function definitions. */
/*************************/

/** Allocate memory for a matrix with the specified number of rows and
 *  columns.
 */
static elem_t* new_matrix(const int rows, const int cols)
{
  assert(rows > 0);
  assert(cols > 0);
  return malloc(rows * cols * sizeof(elem_t));
}

/** Free the memory that was allocated for a matrix. */
static void delete_matrix(elem_t* const a)
{
  free(a);
}

/** Fill in some numbers in a matrix. */
static void init_matrix(elem_t* const a, const int rows, const int cols)
{
  int i, j;
  for (i = 0; i < rows; i++)
  {
    for (j = 0; j < rows; j++)
    {
      a[i * cols + j] = 1.0 / (1 + abs(i-j));
    }
  }
}

/** Print all elements of a matrix. */
void print_matrix(const char* const label,
                  const elem_t* const a, const int rows, const int cols)
{
  int i, j;
  printf("%s:\n", label);
  for (i = 0; i < rows; i++)
  {
    for (j = 0; j < cols; j++)
    {
      printf("%g ", a[i * cols + j]);
    }
    printf("\n");
  }
}

/** Copy a subset of the elements of a matrix into another matrix. */
static void copy_matrix(const elem_t* const from,
                        const int from_rows,
                        const int from_cols,
                        const int from_row_first,
                        const int from_row_last,
                        const int from_col_first,
                        const int from_col_last,
                        elem_t* const to,
                        const int to_rows,
                        const int to_cols,
                        const int to_row_first,
                        const int to_row_last,
                        const int to_col_first,
                        const int to_col_last)
{
  int i, j;

  assert(from_row_last - from_row_first == to_row_last - to_row_first);
  assert(from_col_last - from_col_first == to_col_last - to_col_first);

  for (i = from_row_first; i < from_row_last; i++)
  {
    assert(i < from_rows);
    assert(i - from_row_first + to_row_first < to_rows);
    for (j = from_col_first; j < from_col_last; j++)
    {
      assert(j < from_cols);
      assert(j - from_col_first + to_col_first < to_cols);
      to[(i - from_row_first + to_col_first) * to_cols
         + (j - from_col_first + to_col_first)]
        = from[i * from_cols + j];
    }
  }
}

/** Compute the matrix product of a1 and a2. */
static elem_t* multiply_matrices(const elem_t* const a1,
                                 const int rows1,
                                 const int cols1,
                                 const elem_t* const a2,
                                 const int rows2,
                                 const int cols2)
{
  int i, j, k;
  elem_t* prod;

  assert(cols1 == rows2);

  prod = new_matrix(rows1, cols2);
  for (i = 0; i < rows1; i++)
  {
    for (j = 0; j < cols2; j++)
    {
      prod[i * cols2 + j] = 0;
      for (k = 0; k < cols1; k++)
      {
        prod[i * cols2 + j] += a1[i * cols1 + k] * a2[k * cols2 + j];
      }
    }
  }
  return prod;
}

/** Apply the Gauss-Jordan elimination algorithm on the matrix p->a starting
 *  at row r0 and up to but not including row r1. It is assumed that as many
 *  threads execute this function concurrently as the count barrier p->b was
 *  initialized with. If the matrix p->a is nonsingular, and if matrix p->a
 *  has at least as many columns as rows, the result of this algorithm is that
 *  submatrix p->a[0..p->rows-1,0..p->rows-1] is the identity matrix.
 * @see http://en.wikipedia.org/wiki/Gauss-Jordan_elimination
 */
static void gj_threadfunc(struct gj_threadinfo* p)
{
  int i, j, k;
  elem_t* const a = p->a;
  const int rows = p->rows;
  const int cols = p->cols;

  for (i = 0; i < p->rows; i++)
  {
    if (pthread_barrier_wait(p->b) == PTHREAD_BARRIER_SERIAL_THREAD)
    {
      // Pivoting.
      j = i;
      for (k = i + 1; k < rows; k++)
      {
        if (a[k * cols + i] > a[j * cols + i])
        {
          j = k;
        }
      }
      if (j != i)
      {
        for (k = 0; k < cols; k++)
        {
          const elem_t t = a[i * cols + k];
          a[i * cols + k] = a[j * cols + k];
          a[j * cols + k] = t;
        }
      }
      // Normalize row i.
      if (a[i * cols + i] != 0)
      {
        for (k = cols - 1; k >= 0; k--)
        {
          a[i * cols + k] /= a[i * cols + i];
        }
      }
    }
    pthread_barrier_wait(p->b);
    // Reduce all rows j != i.
    for (j = p->r0; j < p->r1; j++)
    {
      if (i != j)
      {
        const elem_t factor = a[j * cols + i];
        for (k = 0; k < cols; k++)
        {
          a[j * cols + k] -= a[i * cols + k] * factor;
        }
      }
    }
  }
}

/** Multithreaded Gauss-Jordan algorithm. */
static void gj(elem_t* const a, const int rows, const int cols)
{
  int i;
  struct gj_threadinfo* t;
  pthread_barrier_t b;
  pthread_attr_t attr;
  int err;

  assert(rows <= cols);

  t = malloc(sizeof(struct gj_threadinfo) * s_nthread);

  pthread_barrier_init(&b, 0, s_nthread);

  pthread_attr_init(&attr);
  err = pthread_attr_setstacksize(&attr, PTHREAD_STACK_MIN + 4096);
  assert(err == 0);

  for (i = 0; i < s_nthread; i++)
  {
    t[i].b = &b;
    t[i].a = a;
    t[i].rows = rows;
    t[i].cols = cols;
    t[i].r0 = i * rows / s_nthread;
    t[i].r1 = (i+1) * rows / s_nthread;
    pthread_create(&t[i].tid, &attr, (void*(*)(void*))gj_threadfunc, &t[i]);
  }

  pthread_attr_destroy(&attr);

  for (i = 0; i < s_nthread; i++)
  {
    pthread_join(t[i].tid, 0);
  }

  pthread_barrier_destroy(&b);

  free(t);
}

/** Matrix inversion via the Gauss-Jordan algorithm. */
static elem_t* invert_matrix(const elem_t* const a, const int n)
{
  int i, j;
  elem_t* const inv = new_matrix(n, n);
  elem_t* const tmp = new_matrix(n, 2*n);
  copy_matrix(a, n, n, 0, n, 0, n, tmp, n, 2 * n, 0, n, 0, n);
  for (i = 0; i < n; i++)
    for (j = 0; j < n; j++)
      tmp[i * 2 * n + n + j] = (i == j);
  gj(tmp, n, 2*n);
  copy_matrix(tmp, n, 2*n, 0, n, n, 2*n, inv, n, n, 0, n, 0, n);
  delete_matrix(tmp);
  return inv;
}

/** Compute the average square error between the identity matrix and the
 * product of matrix a with its inverse matrix.
 */
static double identity_error(const elem_t* const a, const int n)
{
  int i, j;
  elem_t e = 0;
  for (i = 0; i < n; i++)
  {
    for (j = 0; j < n; j++)
    {
      const elem_t d = a[i * n + j] - (i == j);
      e += d * d;
    }
  }
  return sqrt(e / (n * n));
}

/** Compute epsilon for the numeric type elem_t. Epsilon is defined as the
 *  smallest number for which the sum of one and that number is different of
 *  one. It is assumed that the underlying representation of elem_t uses
 *  base two.
 */
static elem_t epsilon()
{
  elem_t eps;
  for (eps = 1; 1 + eps != 1; eps /= 2)
    ;
  return 2 * eps;
}

int main(int argc, char** argv)
{
  int matrix_size;
  int silent = 0;
  int optchar;
  elem_t *a, *inv, *prod;
  elem_t eps;
  double error;
  double ratio;

  while ((optchar = getopt(argc, argv, "qt:")) != EOF)
  {
    switch (optchar)
    {
    case 'q': silent = 1; break;
    case 't': s_nthread = atoi(optarg); break;
    default:
      fprintf(stderr, "Error: unknown option '%c'.\n", optchar);
      return 1;
    }
  }

  if (optind + 1 != argc)
  {
    fprintf(stderr, "Error: wrong number of arguments.\n");
  }
  matrix_size = atoi(argv[optind]);

  /* Error checking. */
  assert(matrix_size >= 1);
  assert(s_nthread >= 1);

  eps = epsilon();
  a = new_matrix(matrix_size, matrix_size);
  init_matrix(a, matrix_size, matrix_size);
  inv = invert_matrix(a, matrix_size);
  prod = multiply_matrices(a, matrix_size, matrix_size,
                           inv, matrix_size, matrix_size);
  error = identity_error(prod, matrix_size);
  ratio = error / (eps * matrix_size);
  if (! silent)
  {
    printf("error = %g; epsilon = %g; error / (epsilon * n) = %g\n",
           error, eps, ratio);
  }
  if (isfinite(ratio) && ratio < 100)
    printf("Error within bounds.\n");
  else
    printf("Error out of bounds.\n");
  delete_matrix(prod);
  delete_matrix(inv);
  delete_matrix(a);

  return 0;
}
