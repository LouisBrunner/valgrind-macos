
/*--------------------------------------------------------------------*/
/*--- A simple program to listen for valgrind logfile data.        ---*/
/*---                                          valgrind-listener.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2013 Julian Seward 
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

/* Include valgrind headers before system headers to avoid problems
   with the system headers #defining things which are used as names
   of structure members in vki headers. */

#include "pub_core_basics.h"
#include "pub_core_libcassert.h"    // For VG_BUGS_TO
#include "pub_core_vki.h"           // Avoids warnings from 
                                    // pub_core_libcfile.h
#include "pub_core_libcfile.h"      // For VG_CLO_DEFAULT_LOGPORT

#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <time.h>
#include <fcntl.h>
#include <stdlib.h>
#include <signal.h>
#include <sys/poll.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>


/*---------------------------------------------------------------*/

/* The maximum allowable number concurrent connections. */
#define M_CONNECTIONS 50


/*---------------------------------------------------------------*/

__attribute__ ((noreturn))
static void panic ( const char* str )
{
   fprintf(stderr,
           "\nvalgrind-listener: the "
           "'impossible' happened:\n   %s\n", str);
   fprintf(stderr,
           "Please report this bug at: %s\n\n", VG_BUGS_TO);
   exit(1);
}

__attribute__ ((noreturn))
static void my_assert_fail ( const char* expr, const char* file, int line, const char* fn )
{
   fprintf(stderr,
           "\nvalgrind-listener: %s:%d (%s): Assertion '%s' failed.\n",
           file, line, fn, expr );
   fprintf(stderr,
           "Please report this bug at: %s\n\n", VG_BUGS_TO);
   exit(1);
}

#undef assert

#define assert(expr)                                             \
  ((void) ((expr) ? 0 :					         \
	   (my_assert_fail (VG_STRINGIFY(expr),	                 \
                            __FILE__, __LINE__,                  \
                            __PRETTY_FUNCTION__), 0)))


/*---------------------------------------------------------------*/

/* holds the fds for connections; zero if slot not in use. */
int conn_count = 0;
int           conn_fd[M_CONNECTIONS];
struct pollfd conn_pollfd[M_CONNECTIONS];


static void set_nonblocking ( int sd )
{
   int res;
   res = fcntl(sd, F_GETFL);
   res = fcntl(sd, F_SETFL, res | O_NONBLOCK);
   if (res != 0) {
      perror("fcntl failed");
      panic("set_nonblocking");
   }
}

static void set_blocking ( int sd )
{
   int res;
   res = fcntl(sd, F_GETFL);
   res = fcntl(sd, F_SETFL, res & ~O_NONBLOCK);
   if (res != 0) {
      perror("fcntl failed");
      panic("set_blocking");
   }
}


static void copyout ( char* buf, int nbuf )
{
   int i;
   for (i = 0; i < nbuf; i++) {
      if (buf[i] == '\n') {
         fprintf(stdout, "\n(%d) ", conn_count);
      } else {
         __attribute__((unused)) size_t ignored 
            = fwrite(&buf[i], 1, 1, stdout);
      }
   }
   fflush(stdout);
}

static int read_from_sd ( int sd )
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


static void snooze ( void )
{
   struct timespec req;
   req.tv_sec = 0;
   req.tv_nsec = 200 * 1000 * 1000;
   nanosleep(&req,NULL);
}


/* returns 0 if invalid, else port # */
static int atoi_portno ( const char* str )
{
   int n = 0;
   while (1) {
      if (*str == 0) 
         break;
      if (*str < '0' || *str > '9')
         return 0;
      n = 10*n + (int)(*str - '0');
      str++;
      if (n >= 65536) 
         return 0;
   }
   if (n < 1024)
      return 0;
   return n;
}


static void usage ( void )
{
   fprintf(stderr, 
      "\n"
      "usage is:\n"
      "\n"
      "   valgrind-listener [--exit-at-zero|-e] [port-number]\n"
      "\n"
      "   where   --exit-at-zero or -e causes the listener to exit\n"
      "           when the number of connections falls back to zero\n"
      "           (the default is to keep listening forever)\n"
      "\n"
      "           port-number is the default port on which to listen for\n"
      "           connections.  It must be between 1024 and 65535.\n"
      "           Current default is %d.\n"
      "\n"
      ,
      VG_CLO_DEFAULT_LOGPORT
   );
   exit(1);
}


static void banner ( const char* str )
{
   time_t t;
   t = time(NULL);
   printf("valgrind-listener %s at %s", str, ctime(&t));
   fflush(stdout);
}


static void exit_routine ( void )
{
   banner("exited");
   exit(0);
}


static void sigint_handler ( int signo )
{
   exit_routine();
}


int main (int argc, char** argv) 
{
   int    i, j, k, res, one;
   int    main_sd, new_sd;
   socklen_t client_len;
   struct sockaddr_in client_addr, server_addr;

   char /*bool*/ exit_when_zero = 0;
   int           port = VG_CLO_DEFAULT_LOGPORT;

   for (i = 1; i < argc; i++) {
      if (0==strcmp(argv[i], "--exit-at-zero")
          || 0==strcmp(argv[i], "-e")) {
         exit_when_zero = 1;
      }
      else
      if (atoi_portno(argv[i]) > 0) {
         port = atoi_portno(argv[i]);
      }
      else
      usage();
   }

   banner("started");
   signal(SIGINT, sigint_handler);

   conn_count = 0;
   for (i = 0; i < M_CONNECTIONS; i++)
      conn_fd[i] = 0;

   /* create socket */
   main_sd = socket(AF_INET, SOCK_STREAM, 0);
   if (main_sd < 0) {
      perror("cannot open socket ");
      panic("main -- create socket");
   }

   /* allow address reuse to avoid "address already in use" errors */

   one = 1;
   if (setsockopt(main_sd, SOL_SOCKET, SO_REUSEADDR, 
		  &one, sizeof(int)) < 0) {
      perror("cannot enable address reuse ");
      panic("main -- enable address reuse");
   }

   /* bind server port */
   server_addr.sin_family      = AF_INET;
   server_addr.sin_addr.s_addr = htonl(INADDR_ANY);
   server_addr.sin_port        = htons(port);
  
   if (bind(main_sd, (struct sockaddr *) &server_addr, 
                     sizeof(server_addr) ) < 0) {
      perror("cannot bind port ");
      panic("main -- bind port");
   }

   res = listen(main_sd,M_CONNECTIONS);
   if (res != 0) {
      perror("listen failed ");
      panic("main -- listen");
   }
  
   while (1) {

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
           client_len = sizeof(client_addr);
           new_sd = accept(main_sd, (struct sockaddr *)&client_addr, 
                                                       &client_len);
           if (new_sd < 0) {
              perror("cannot accept connection ");
              panic("main -- accept connection");
           }

           /* find a place to put it. */
	   assert(new_sd > 0);
           for (i = 0; i < M_CONNECTIONS; i++)
              if (conn_fd[i] == 0) 
                 break;

           if (i >= M_CONNECTIONS) {
              fprintf(stderr, "Too many concurrent connections.  "
                              "Increase M_CONNECTIONS and recompile.\n");
              panic("main -- too many concurrent connections");
           }

           conn_fd[i] = new_sd;
           conn_count++;
	   printf("\n(%d) -------------------- CONNECT "
                  "--------------------\n(%d)\n(%d) ", 
                  conn_count, conn_count, conn_count);
           fflush(stdout);
        } /* while (1) */
      }

      /* We've processed all new connect requests.  Listen for changes
         to the current set of fds. */
      j = 0;
      for (i = 0; i < M_CONNECTIONS; i++) {
         if (conn_fd[i] == 0)
            continue;
         conn_pollfd[j].fd = conn_fd[i];
         conn_pollfd[j].events = POLLIN /* | POLLHUP | POLLNVAL */;
         conn_pollfd[j].revents = 0;
         j++;
      }

      res = poll(conn_pollfd, j, 0 /* return immediately. */ );
      if (res < 0) {
         perror("poll(main) failed");
         panic("poll(main) failed");
      }
    
      /* nothing happened. go round again. */
      if (res == 0) {
         continue;
      }

      /* inspect the fds. */
      for (i = 0; i < j; i++) {
 
         if (conn_pollfd[i].revents & POLLIN) {
            /* data is available on this fd */
            res = read_from_sd(conn_pollfd[i].fd);
 
            if (res == 0) {
               /* the connection has been closed. */
               close(conn_pollfd[i].fd);
               /* this fd has been closed or otherwise gone bad; forget
                 about it. */
               for (k = 0; k < M_CONNECTIONS; k++)
                  if (conn_fd[k] == conn_pollfd[i].fd) 
                     break;
               assert(k < M_CONNECTIONS);
               conn_fd[k] = 0;
               conn_count--;
               printf("\n(%d) ------------------- DISCONNECT "
                      "-------------------\n(%d)\n(%d) ", 
                      conn_count, conn_count, conn_count);
               fflush(stdout);
               if (conn_count == 0 && exit_when_zero) {
                  printf("\n");
                  fflush(stdout);
                  exit_routine();
	       }
            }
         }

      } /* for (i = 0; i < j; i++) */
  
   } /* while (1) */

   /* NOTREACHED */
}


/*--------------------------------------------------------------------*/
/*--- end                                      valgrind-listener.c ---*/
/*--------------------------------------------------------------------*/
