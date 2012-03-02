#include <config.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>

#ifdef HAVE_MQUEUE_H

#include <mqueue.h>

#define MSGMAX 10
#define MSGSIZEMAX 1024

int main(int argc, char **argv)
{
  struct mq_attr mqa;
  mqd_t mqdw;
  mqd_t mqdr;
  char buffer[MSGSIZEMAX];
  unsigned int priority;
  int len;

  mqa.mq_maxmsg = MSGMAX;
  mqa.mq_msgsize = MSGSIZEMAX;
  
  if ((mqdw = mq_open("/valgrind-mqueue", O_CREAT|O_EXCL|O_WRONLY, 0600, &mqa)) < 0)
    {
      if (errno == ENOSYS)
        exit(0);
      perror("mq_open");
      exit(1);
    }

  if ((mqdr = mq_open("/valgrind-mqueue", O_RDONLY)) < 0)
    {
      perror("mq_open");
      mq_unlink("/valgrind-mqueue");
      mq_close(mqdw);
      exit(1);
    }
  
  if (mq_unlink("/valgrind-mqueue") < 0)
    {
      perror("mq_unlink");
      mq_close(mqdw);
      mq_close(mqdr);
      exit(1);
    }

  if (mq_send(mqdw, "PING", 4, 0) < 0)
    {
      perror("mq_send");
      mq_close(mqdr);
      mq_close(mqdw);
      exit(1);
    }

  if ((len = mq_receive(mqdr, buffer, sizeof(buffer), &priority)) < 0)
    {
      perror("mq_receive");
      mq_close(mqdr);
      mq_close(mqdw);
      exit(1);
    }

  if (len != 4 || memcmp(buffer, "PING", 4) != 0)
    {
      fprintf(stderr, "Message corrupt!");
    }

  if (mq_notify(mqdr, NULL) < 0)
    {
      perror("mq_notify");
      mq_close(mqdr);
      mq_close(mqdw);
      exit(1);
    }

  if (mq_getattr(mqdr, &mqa) < 0)
    {
      perror("mq_getattr");
      mq_close(mqdr);
      mq_close(mqdw);
      exit(1);
    }

  if (mq_setattr(mqdw, &mqa, &mqa) < 0)
    {
      perror("mq_setattr");
      mq_close(mqdr);
      mq_close(mqdw);
      exit(1);
    }

  if (mq_close(mqdr) < 0)
    {
      perror("mq_close");
      mq_close(mqdw);
      exit(1);
    }
  
  if (mq_close(mqdw) < 0)
    {
      perror("mq_close");
      exit(1);
    }

  exit(0);
}

#else

int main(int argc, char **argv)
{
  exit(0);
}

#endif
