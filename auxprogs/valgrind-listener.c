
/*--------------------------------------------------------------------*/
/*--- A simple program to listen for valgrind logfile data.        ---*/
/*---                                          valgrind-listener.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, an extensible x86 protected-mode
   emulator for monitoring program execution on x86-Unixes.

   Copyright (C) 2000-2002 Julian Seward 
      jseward@acm.org

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307, USA.

   The GNU General Public License is contained in the file COPYING.
*/


/*---------------------------------------------------------------*/

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <stdio.h>
#include <unistd.h> /* close */
#include <string.h>
#include <assert.h>
#include <sys/poll.h>
#include <time.h>
#include <fcntl.h>
#include <stdlib.h>

/*---------------------------------------------------------------*/

#define SUCCESS 0
#define ERROR   1

#define END_LINE '\n'
#define SERVER_PORT 1500
#define MAX_CONNS 50

/*---------------------------------------------------------------*/

/* holds the fds for connections; zero if slot not in use. */
int conn_count = 0;
int           conn_fd[MAX_CONNS];
struct pollfd conn_pollfd[MAX_CONNS];


void set_nonblocking ( int sd )
{
   int res;
   res = fcntl(sd, F_GETFL);
   res = fcntl(sd, F_SETFL, res | O_NONBLOCK);
   if (res != 0) {
      perror("fcntl failed");
      exit(ERROR);
   }
}

void set_blocking ( int sd )
{
   int res;
   res = fcntl(sd, F_GETFL);
   res = fcntl(sd, F_SETFL, res & ~O_NONBLOCK);
   if (res != 0) {
      perror("fcntl failed");
      exit(ERROR);
   }
}


void copyout ( char* buf, int nbuf )
{
   int i;
   for (i = 0; i < nbuf; i++) {
     if (buf[i] == '\n') {
       fprintf(stdout, "\n(%d) ", conn_count);
     } else {
       fwrite(&buf[i], 1, 1, stdout);
     }
   }
   fflush(stdout);
}

int read_from_sd ( int sd )
{
  char buf[100];
  int n;

  set_blocking(sd);
  n = read(sd, buf, 99);
  if (n <= 0) return 0; /* closed */
  copyout(buf, n);

  set_nonblocking(sd);
  while (1) {
     n = read(sd, buf, 100);
     if (n <= 0) return 1; /* not closed */
     copyout(buf, n);
  }
}


void snooze ( void )
{
  struct timespec req;
  req.tv_sec = 0;
  req.tv_nsec = 200 * 1000 * 1000;
  nanosleep(&req,NULL);
}


/* function readline */
int read_line();

int main (int argc, char *argv[]) {
  int i, j, k, res;
  int main_sd, newSd, cliLen;

  struct sockaddr_in cliAddr, servAddr;

  conn_count = 0;
  for (i = 0; i < MAX_CONNS; i++)
     conn_fd[i] = 0;

  /* create socket */
  main_sd = socket(AF_INET, SOCK_STREAM, 0);
   if (main_sd < 0) {
    perror("cannot open socket ");
    return ERROR;
  }
  
  /* bind server port */
  servAddr.sin_family = AF_INET;
  servAddr.sin_addr.s_addr = htonl(INADDR_ANY);
  servAddr.sin_port = htons(SERVER_PORT);
  
  if(bind(main_sd, (struct sockaddr *) &servAddr, sizeof(servAddr))<0) {
    perror("cannot bind port ");
    return ERROR;
  }

  res = listen(main_sd,MAX_CONNS);
  if (res != 0) {
    perror("listen failed ");
    return ERROR;
  }
  
  while(1) {

    snooze();

    /* enquire, using poll, whether there is any activity available on
       the main socket descriptor.  If so, someone is trying to
       connect; get the fd and add it to our table thereof. */
    { struct pollfd ufd;
      while (1) {
         ufd.fd = main_sd;
         ufd.events = POLLIN;
         ufd.revents = 0;
         res = poll(&ufd, 1, 0);
         if (res == 0) break;

	 /* ok, we have someone waiting to connect.  Get the sd. */
         cliLen = sizeof(cliAddr);
         newSd = accept(main_sd, (struct sockaddr *) &cliAddr, &cliLen);
         if(newSd<0) {
            perror("cannot accept connection ");
            return ERROR;
         }

	 /* find a place to put it. */
	 assert(newSd > 0);
	 for (i = 0; i < MAX_CONNS; i++)
            if (conn_fd[i] == 0) 
               break;

         if (i >= MAX_CONNS) {
	   printf("too many concurrent connections\n");
	   return ERROR;
	 }
         conn_fd[i] = newSd;
	 conn_count++;
	 printf("\n(%d) -------------------- CONNECT "
                "--------------------\n(%d)\n(%d) ", 
                conn_count, conn_count, conn_count);
         fflush(stdout);
      }
    }


    /* We've processed all new connect requests.  Listen for changes
       to the current set of fds. */
    j = 0;
    for (i = 0; i < MAX_CONNS; i++) {
      if (conn_fd[i] == 0)
	continue;
      conn_pollfd[j].fd = conn_fd[i];
      conn_pollfd[j].events = POLLIN | POLLHUP | POLLNVAL;
      conn_pollfd[j].revents = 0;
      j++;
    }

    res = poll(conn_pollfd, j, 0 /* return immediately. */ );

    if (res < 0) {
      perror("poll(main) failed");
      return ERROR;
    }
    
    /* nothing happened. go round again. */
    if (res == 0) {
       continue;
    }

    /* inspect the fds. */
    for (i = 0; i < j; i++) {

      if ((conn_pollfd[i].revents & POLLHUP) 
          || (conn_pollfd[i].revents & POLLNVAL)) {
	/* this fd has been closed or otherwise gone bad; forget about
           it. */
      closed:
	for (k = 0; k < MAX_CONNS; k++)
	  if (conn_fd[k] == conn_pollfd[i].fd) break;
	assert(k < MAX_CONNS);
	conn_fd[k] = 0;
	 conn_count--;
	printf("\n(%d) ------------------- DISCONNECT "
               "-------------------\n(%d)\n(%d) ", 
               conn_count, conn_count, conn_count);
         fflush(stdout);
      }
      else 
      if (conn_pollfd[i].revents & POLLIN) {
	/* data is available on this fd */
	res = read_from_sd(conn_pollfd[i].fd);
	if (res == 0) goto closed;
      }

    } /* for (i = 0; i < j; i++) */


    
  } /* while (1) */

}


/*--------------------------------------------------------------------*/
/*--- end                                      valgrind-listener.c ---*/
/*--------------------------------------------------------------------*/
