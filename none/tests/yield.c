#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

static pthread_mutex_t m_go;
static pthread_cond_t c_go;

static volatile int alive;

static int sch_yield;
static int rep_nop;

static void *th1(void *v)
{
	pthread_mutex_lock(&m_go);
	while(!alive)
		pthread_cond_wait(&c_go, &m_go);
	pthread_mutex_unlock(&m_go);

	while(alive) {
		sch_yield++;
		sched_yield();
	}

	return 0;
}

static void *th2(void *v)
{
	pthread_mutex_lock(&m_go);
	while(!alive)
		pthread_cond_wait(&c_go, &m_go);
	pthread_mutex_unlock(&m_go);

	while(alive) {
		rep_nop++;
		asm volatile ("rep; nop" : : : "memory");
	}

	return 0;
}

int main()
{
	pthread_t a, b;

	pthread_create(&a, NULL, th1, NULL);
	pthread_create(&b, NULL, th2, NULL);

	/* make sure both threads start at the same time */
	pthread_mutex_lock(&m_go);
	alive = 1;
	pthread_cond_signal(&c_go);
	pthread_mutex_unlock(&m_go);

	sleep(1);

	alive = 0;
	pthread_join(a, NULL);
	pthread_join(b, NULL);

	if (abs(sch_yield - rep_nop) < 2)
		printf("PASS\n");
	else
		printf("FAIL sch_yield=%d rep_nop=%d\n", 
		       sch_yield, rep_nop);

	return 0;
}
