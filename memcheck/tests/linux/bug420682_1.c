/*
 * See https://bugs.kde.org/show_bug.cgi?id=420682
 *
 */
#include <assert.h>
#include <fcntl.h>
#include <libaio.h>
#include <unistd.h>

int main(void)
{
    const char *msg = "hello world\n";
    struct iocb iocb = {};
    struct io_event event;
    io_context_t ctx = 0;
    struct iocb *iocbp;
    int rc, fd;

    rc = io_setup(1, &ctx);
    assert(rc == 0);

    fd = open("test.txt", O_CREAT | O_RDWR, 0666);
    assert(fd >= 0);

    io_prep_pwrite(&iocb, fd, (void *)msg, 12, 0);
    iocbp = &iocb;

    rc = io_submit(ctx, 1, &iocbp);
    assert(rc == 1);

    rc = io_getevents(ctx, 1, 1, &event, NULL);
    assert(rc == 1);

    close(fd);

    io_destroy(ctx);
}
