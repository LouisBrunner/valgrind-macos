/** Compute the matrix inverse via Gauss-Jordan elimination.
 *  This program uses OpenMP separate computation steps but no
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
#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>  // getopt()


/*********************/
/* Type definitions. */
/*********************/

typedef double elem_t;


/********************/
/* Local variables. */
/********************/

static int s_trigger_race;


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
static void gj(elem_t* const a, const int rows, const int cols)
{
  int i, j, k;

  for (i = 0; i < rows; i++)
  {
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

    // Reduce all rows j != i.

    if (s_trigger_race)
    {
#     pragma omp parallel for private(j)
      for (j = 0; j < rows; j++)
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
    else
    {
#     pragma omp parallel for private(j, k)
      for (j = 0; j < rows; j++)
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

static void usage(const char* const exe)
{
  printf("Usage: %s [-h] [-q] [-r] [-t<n>] <m>\n"
         "-h: display this information.\n"
         "-q: quiet mode -- do not print computed error.\n"
         "-r: trigger a race condition.\n"
         "-t<n>: use <n> threads.\n"
         "<m>: matrix size.\n",
         exe);
}

int main(int argc, char** argv)
{
  int matrix_size;
  int nthread = 1;
  int silent = 0;
  int optchar;
  elem_t *a, *inv, *prod;
  elem_t eps;
  double error;
  double ratio;

  while ((optchar = getopt(argc, argv, "hqrt:")) != EOF)
  {
    switch (optchar)
    {
    case 'h': usage(argv[0]); return 1;
    case 'q': silent = 1; break;
    case 'r': s_trigger_race = 1; break;
    case 't': nthread = atoi(optarg); break;
    default:
      return 1;
    }
  }

  if (optind + 1 != argc)
  {
    fprintf(stderr, "Error: wrong number of arguments.\n");
    return 1;
  }
  matrix_size = atoi(argv[optind]);

  /* Error checking. */
  assert(matrix_size >= 1);
  assert(nthread >= 1);

  omp_set_num_threads(nthread);
  omp_set_dynamic(0);

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
