#include <arpa/inet.h>
#include <sys/ioctl.h>
#include <sys/socket.h>
#include <unistd.h>

int main()
{
   pid_t pid;
   int fd, atmark;

   fd = socket(PF_INET, SOCK_DGRAM, 0);

   ioctl(fd, SIOCSPGRP, &pid);
   ioctl(fd, SIOCATMARK, &atmark);

   close(fd);

   return 0;
}
