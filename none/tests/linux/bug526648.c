#include <stdio.h>
#include <pty.h>
#include <unistd.h>
#include <sys/wait.h>

int main()
{
    int masterFd = 0;
    FILE *devnull = fopen("/dev/null", "w");

    pid_t pid = forkpty(&masterFd, NULL, NULL,  NULL);
    if(pid > 0) {

        // Prevent optimizing out
        fprintf(devnull, "%d\n", masterFd);
        fclose(devnull);

        close(masterFd);
        waitpid(pid, NULL, 0);
    } else if (pid == 0) {
        _exit(0);
    }
    return 0;
}
