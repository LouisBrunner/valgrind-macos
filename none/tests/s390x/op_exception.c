/* zArchitecture specifies that operation exception(illegal opcode) is
suppressing. That means that the program check old psw will point to
the instruction after the illegal one (according to the calculated length).
There are some programs out there that use this mechanism to detect available
intruction (sigh).
This patch checks, that valgrind makes forard progress. */
#include <signal.h>
#include <stdio.h>
#include <string.h>
static volatile int got_ill;
static void handle_ill(int sig)
{
        got_ill = 1;
}
int main()
{
	struct sigaction sa;

	memset(&sa, 0, sizeof(sa));
	sa.sa_handler = handle_ill;
	sigaction(SIGILL, &sa, NULL);

	got_ill = 0;
	/* most architectures loop here, but on s390 this would increase the
	   PSW by 2 and then by 2 */
        asm volatile(".long 0\n");
        if (got_ill)
                printf("0x00000000 does not loop\n");

	got_ill = 0;
	/* most architectures loop here, but on s390 this would increase the
	   PSW by 6 and then by 2*/
        asm volatile(".long 0xffffffff\n.long 0xffff0000\n");
        if (got_ill)
                printf("0xffffffff does not loop\n");

}

