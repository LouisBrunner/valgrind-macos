#include <fcntl.h>
#include <signal.h>
#include <stdio.h>
#include <sys/time.h>
#include <unistd.h>

volatile int ticks = 0;
struct itimerval timert;
struct sigaction timer_action;


void handle_vtalrm(int sig) {
  ticks++;
}

void setup_timer() {
  timer_action.sa_handler = handle_vtalrm;
  sigemptyset(&timer_action.sa_mask);
  timer_action.sa_flags = SA_RESTART;

  sigaction(SIGVTALRM, &timer_action, NULL);

  timert.it_interval.tv_sec = timert.it_value.tv_sec = 0;
  timert.it_interval.tv_usec = timert.it_value.tv_usec = 100;
  setitimer(ITIMER_VIRTUAL, &timert, NULL);
}

int main(int argc, char *argv[]) {
  setup_timer();
  sleep(5);
}
