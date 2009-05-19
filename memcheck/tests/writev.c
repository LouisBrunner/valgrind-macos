#include <stdio.h>
#include <sys/types.h>
#include <sys/uio.h>
#include <fcntl.h>
#include <errno.h>
#include <unistd.h>
#include <string.h>

#define K_1             8192
#define NBUFS           2
#define CHUNK           K_1             /* single chunk */
#define MAX_IOVEC       2
#define DATA_FILE       "writev_data_file"

static char    buf1[K_1];
static char    buf2[K_1];
static char    *buf_list[NBUFS], f_name[]="writev_data_file";
static int	fd;
 
struct  iovec   wr_iovec[MAX_IOVEC] = {
        {(caddr_t)-1,   CHUNK},
        {(caddr_t)NULL, 0}
};

int main(void)
{
	int nbytes;

	/* Fill the buf_list[0] and buf_list[1] with 0 zeros */
        buf_list[0] = buf1;
        buf_list[1] = buf2;
        memset(buf_list[0], 0, K_1);
        memset(buf_list[1], 0, K_1);
                                                                                
        if ((fd = open(f_name, O_WRONLY | O_CREAT, 0666)) < 0) {
             fprintf(stderr, "open(2) failed: fname = %s, errno = %d\n",
			 f_name, errno);
		return 1;
        } else if ((nbytes = write(fd, buf_list[1], K_1)) != K_1) {
		fprintf(stderr, "write(2) failed: nbytes = %d, errno = %d\n",
			 nbytes, errno);
                return 1;
        }
        if (close(fd) < 0) {
        	fprintf(stderr, "close failed: errno = %d\n", errno);
                return 1;
	}
        fprintf(stderr, "Test file created.\n"); 
        if ((fd = open(f_name, O_RDWR, 0666)) < 0) {
               	fprintf(stderr, "open failed: fname = %s, errno = %d\n",
                        f_name, errno);
                return 1;
	}
 
        lseek(fd, 0, 0);
        if (writev(fd, wr_iovec, 2) < 0) {
		if (errno == EFAULT) 
                	fprintf(stderr, "Received EFAULT as expected\n");
                else 
                	fprintf(stderr, "Expected EFAULT, got %d\n", errno);
                lseek(fd, K_1, 0);
                if ((nbytes = read(fd, buf_list[0], CHUNK)) != 0) 
                	fprintf(stderr, "Expected nbytes = 0, got %d\n", nbytes);
        } 
	else 
        	fprintf(stderr, "Error writev returned a positive value\n");
	// Now check invalid vector count
        if (writev(fd, wr_iovec, -1) < 0) {
 		if (errno == EINVAL) 
                	fprintf(stderr, "Received EINVAL as expected\n");
                else 
                	fprintf(stderr, "expected errno = EINVAL, got %d\n", errno);
 	}
	else 
        	fprintf(stderr, "Error writev returned a positive value\n");
        if (readv(fd, wr_iovec, -1) < 0) {
 		if (errno == EINVAL) 
                	fprintf(stderr, "Received EINVAL as expected\n");
                else 
                	fprintf(stderr, "expected errno = EINVAL, got %d\n", errno);
 	}
	else 
        	fprintf(stderr, "Error writev returned a positive value\n");

        unlink(f_name);
        
	return 0;
}

