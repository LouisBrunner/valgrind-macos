/* Tests sendfilev with bogus inputs. */

#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <string.h>
#include <strings.h>
#include <unistd.h>
#include <sys/sendfile.h>

#define CHUNK (8 * 1024)
#define TEST_FILE "sendfile.test"

int main(int argc, const char *argv[])
{
   int test_fd = open(TEST_FILE, O_WRONLY | O_CREAT, 0666);
   if (test_fd < 0) {
      int error = errno;
      fprintf(stderr, "open failed: %s (%d).\n", strerror(error), error);
      return 1;
   }

   char chunk1[CHUNK];
   bzero(&chunk1, sizeof(chunk1));
   ssize_t nbytes = write(test_fd, &chunk1, sizeof(chunk1));
   if (nbytes != CHUNK) {
      int error = errno;
      fprintf(stderr, "write failed (nbytes=%zd): %s (%d).\n",
              nbytes, strerror(error), error);
      return 1;
   }

   close(test_fd);
   printf("Test file created.\n"); 

   test_fd = open(TEST_FILE, O_RDWR, 0666);
   if (test_fd < 0) {
      int error = errno;
      fprintf(stderr, "open failed: %s (%d).\n", strerror(error), error);
      return 1;
   }

   sendfilevec_t vec[2];
   vec[0].sfv_fd = SFV_FD_SELF;
   vec[0].sfv_off = -1;
   vec[0].sfv_len = 1;
   vec[0].sfv_flag = 0;
   vec[1].sfv_fd = test_fd;
   vec[1].sfv_off = 0;
   vec[1].sfv_len = CHUNK;
   vec[1].sfv_flag = 0;
   size_t xferred;

   nbytes = sendfilev(test_fd, vec, 2, &xferred);
   if (nbytes < 0) {
      if (errno == EFAULT) {
         printf("Received EFAULT as expected.\n");
      } else {
         fprintf(stderr, "Expected EFAULT, got %d.\n", errno);
      }
   } else {
      fprintf(stderr, "Error: sendfilev returned a positive value.\n");
   }

   nbytes = sendfilev(test_fd, vec, -1, &xferred);
   if (nbytes < 0) {
      if (errno == EINVAL) {
         printf("Received EINVAL as expected.\n");
      } else {
         fprintf(stderr, "Expected EINVAL, got %d.\n", errno);
      }
   } else {
      fprintf(stderr, "Error: sendfilev returned a positive value.\n");
   }

   vec[0].sfv_off = (off_t) "HEADER";
   vec[0].sfv_len = 6;
   nbytes = sendfilev(test_fd, vec, 1, &xferred);
   if (nbytes < 0) {
      int error = errno;
      fprintf(stderr, "sendfilev failed: %s (%d).\n", strerror(error), error);
   } else {
      printf("sendfilev for the first buffer succeeded.\n");
   }

   unlink(TEST_FILE);
   return 0;
}
