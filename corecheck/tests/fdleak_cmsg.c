#include <sys/types.h>
#include <sys/wait.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <stdio.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdlib.h>

char filea[24];
char fileb[24];
char sock[24];

void
server ()
{
   int s, fd1, fd2;
   struct sockaddr_un addr;
   
   fd1 = open(filea, O_RDWR | O_CREAT | O_TRUNC, 0750);
   if(fd1 == -1) {
      perror("open");
      exit(1);
   }

   fd2 = open(fileb, O_RDWR | O_CREAT | O_TRUNC, 0750);
   if(fd2 == -1) {
      perror("open");
      exit(1);
   }

   s = socket(PF_UNIX, SOCK_STREAM, 0);
   if(s == -1) {
      perror("socket");
      exit(1);
   }

   memset(&addr, 0, sizeof(addr));
   addr.sun_family = AF_UNIX;
   sprintf(addr.sun_path, sock);

   unlink(addr.sun_path);
   if(bind(s, (struct sockaddr *)&addr, sizeof(addr)) == -1) {
      perror("bind");
      exit(1);
   }

   if(listen(s, 5) == -1) {
      perror("listen");
      exit(1);
   }

   {
      int x;
      int baddrsize = 0;
      struct sockaddr_un baddr;
      struct msghdr msg = {NULL, 0, NULL, 0, 0, 0, 0};
      struct cmsghdr *cmsg;
      char buf[CMSG_SPACE(sizeof(int) * 2)];
      struct iovec iov[1];

      memset(&baddr, 0, sizeof(baddr));
      x = accept(s, (struct sockaddr *)&baddr, &baddrsize);
      if(x == -1) {
         perror("accept");
         exit(1);
      }

      msg.msg_control = buf;
      msg.msg_controllen = sizeof(buf);
      cmsg = CMSG_FIRSTHDR(&msg);
      cmsg->cmsg_level = SOL_SOCKET;
      cmsg->cmsg_type = SCM_RIGHTS;
      cmsg->cmsg_len = CMSG_LEN(sizeof(int) * 2);
      ((int *)CMSG_DATA(cmsg))[0] = fd1;
      ((int *)CMSG_DATA(cmsg))[1] = fd2;

      iov[0].iov_base = "hello";
      iov[0].iov_len = 6;

      msg.msg_iov = iov;
      msg.msg_iovlen = 1;

      if(sendmsg(x, &msg, 0) == -1) {
         perror("sendmsg");
         exit(1);
      }
   }
}

void
client ()
{
   int s, fd1 = -1, fd2 = -1, size, count = 0, ret;
   struct sockaddr_un addr;
   struct iovec iov[1];
   union {
      struct cmsghdr cm;
      char control[CMSG_SPACE(sizeof(int) * 2)];
   } control_un;
   struct msghdr msg = { NULL, 0, iov, 1, control_un.control,
                         sizeof(control_un), 0 };
   struct cmsghdr *cmsg = &control_un.cm;
   char buf[1024];

   iov[0].iov_base = buf;
   iov[0].iov_len = sizeof(buf);

   s = socket(PF_UNIX, SOCK_STREAM, 0);
   if(s == -1) {
      perror("socket");
      exit(1);
   }

   addr.sun_family = AF_UNIX;
   sprintf(addr.sun_path, sock);

   do {
     count++;
     ret = connect(s, (struct sockaddr *)&addr, sizeof(addr));
     if(ret == -1) sleep(1);
   } while (count < 10 && ret == -1);

   if(ret == -1) {
      perror("connect");
      exit(1);
   }

   if((size = recvmsg(s, &msg, 0)) == -1) {
      perror("recvmsg");
      exit(1);
   }


   cmsg = CMSG_FIRSTHDR(&msg);
   while(cmsg) {
      if(cmsg->cmsg_level == SOL_SOCKET &&
         cmsg->cmsg_type == SCM_RIGHTS &&
         cmsg->cmsg_len == CMSG_LEN(sizeof(int) * 2)) {
         fd1 = ((int *)CMSG_DATA(cmsg))[0];
         fd2 = ((int *)CMSG_DATA(cmsg))[1];
      }

      cmsg = CMSG_NXTHDR(&msg, cmsg);
   }

   if(fd1 != -1) write(fd1, "Yeah 1\n", 8);
   if(fd2 != -1) write(fd2, "Yeah 2\n", 8);
}


int
main (int argc, char **argv)
{
   int pid, status;

   /*
    * Fedora Core 1's Perl opens /dev/pts/2 as fd 10.  Let's close it
    * now to get consistent results across different releases.
    */

   close(10);

   pid = getpid();
   sprintf(filea, "/tmp/data1.%d", pid);
   sprintf(fileb, "/tmp/data2.%d", pid);
   sprintf(sock, "/tmp/sock.%d", pid);

   if((pid = fork()) == 0) {
      server();
      return 0;
   }

   client();

   wait(&status);

   unlink(filea);
   unlink(fileb);
   unlink(sock);
   return 0;
}
