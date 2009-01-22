/* 
   Check that a thread which yields with pause (rep;nop) makes less
   progress against a pure spinner.
 */
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

static pthread_mutex_t m_go = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t c_go = PTHREAD_COND_INITIALIZER;
static pthread_cond_t c_running = PTHREAD_COND_INITIALIZER;

static volatile int alive, running;

static int spin;
static int rep_nop;

static void *spinner(void *v)
{
	pthread_mutex_lock(&m_go);
	while(!alive)
		pthread_cond_wait(&c_go, &m_go);
	running++;
	pthread_cond_signal(&c_running);
	pthread_mutex_unlock(&m_go);

	while(alive)
		spin++;

	return 0;
}

static void *rep_nopper(void *v)
{
	pthread_mutex_lock(&m_go);
	while(!alive)
		pthread_cond_wait(&c_go, &m_go);
	running++;
	pthread_cond_signal(&c_running);
	pthread_mutex_unlock(&m_go);

	while(alive) {
		rep_nop++;
                // This gives a hint to a P4, telling it to pause 
                // (ie. we're in a spin-wait loop)
		asm volatile ("rep; nop" : : : "memory");
	}

	return 0;
}

int main()
{
	pthread_t a, b;

	pthread_create(&a, NULL, spinner, NULL);
	pthread_create(&b, NULL, rep_nopper, NULL);

	/* make sure both threads start at the same time */
	pthread_mutex_lock(&m_go);
	alive = 1;
	pthread_cond_broadcast(&c_go);

	/* make sure they both get started */
	while(running < 2)
		pthread_cond_wait(&c_running, &m_go);
	pthread_mutex_unlock(&m_go);

	sleep(2);

	alive = 0;
	pthread_join(a, NULL);
	pthread_join(b, NULL);

	if (0)
		printf("spin=%d rep_nop=%d rep_nop:spin ratio: %g\n", 
		       spin, rep_nop, (float)rep_nop / spin);

	if (spin > rep_nop)
		printf("PASS\n");
	else
		printf("FAIL spin=%d rep_nop=%d rep_nop:spin ratio: %g\n", 
		       spin, rep_nop, (float)rep_nop / spin);

	return 0;
}
