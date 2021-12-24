/*
 * Test this family of functions
 *    extattr_get_fd, extattr_set_fd, extattr_delete_fd, extattr_list_fd,
 *    extattr_get_file, extattr_set_file, extattr_delete_file,
 *    extattr_list_file, extattr_get_link, extattr_set_link,
 *    extattr_delete_link, extattr_list_link - system calls to manipulate VFS
 */

#include <sys/types.h>
#include <sys/extattr.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>
#include "../../memcheck.h"

int main()
{
    ssize_t n;
    char buff[64];
    char tmpfile[] = "test1.XXXXXX";
    int tmpfd = mkstemp(tmpfile);
    memset(buff, 0, sizeof(buff));
    sprintf(buff, "some data");
 
    // valid calls even though "test1" does not exist
    n = extattr_get_file("test1", EXTATTR_NAMESPACE_USER, "bar", NULL, 0);
    (void)extattr_get_file("test1", EXTATTR_NAMESPACE_USER, "bar", buff, sizeof(buff));
    (void)extattr_get_link("test1", EXTATTR_NAMESPACE_USER, "bar", buff, sizeof(buff));
    (void)extattr_list_file("test1", EXTATTR_NAMESPACE_USER, buff, sizeof(buff));
    (void)extattr_list_link("test1", EXTATTR_NAMESPACE_USER, buff, sizeof(buff));
 
    n = extattr_get_fd(tmpfd, EXTATTR_NAMESPACE_USER, "bar", NULL, 0);
    (void)extattr_get_fd(tmpfd, EXTATTR_NAMESPACE_USER, "bar", buff, sizeof(buff));
    (void)extattr_list_fd(tmpfd, EXTATTR_NAMESPACE_USER, buff, sizeof(buff));
 
    n = extattr_set_file("test1", EXTATTR_NAMESPACE_USER, "bar", NULL, 0);
    (void)extattr_set_file("test1", EXTATTR_NAMESPACE_USER, "bar", buff, sizeof(buff));
    (void)extattr_set_link("test1", EXTATTR_NAMESPACE_USER, "bar", buff, sizeof(buff));
 
    n = extattr_set_fd(tmpfd, EXTATTR_NAMESPACE_USER, "bar", NULL, 0);
    (void)extattr_set_fd(tmpfd, EXTATTR_NAMESPACE_USER, "bar", buff, sizeof(buff));
 
    (void)extattr_delete_file("test1", EXTATTR_NAMESPACE_USER, "foo");
    (void)extattr_delete_link("test1", EXTATTR_NAMESPACE_USER, "foo");
    (void)extattr_delete_fd(tmpfd, EXTATTR_NAMESPACE_USER, "foo");
 
    // now some invalid calls
    int uninit;
    (void)extattr_get_file("test1", uninit, "bar", buff, sizeof(buff));
    (void)extattr_get_link("test1", uninit, "bar", buff, sizeof(buff));
    (void)extattr_get_fd(tmpfd, uninit, "bar", buff, sizeof(buff));
    (void)extattr_list_file("test1", uninit, buff, sizeof(buff));
    (void)extattr_list_link("test1", uninit, buff, sizeof(buff));
    (void)extattr_list_fd(tmpfd, uninit, buff, sizeof(buff));
    (void)extattr_delete_file("test1", uninit, "foo");
    (void)extattr_delete_link("test1", uninit, "foo");
    (void)extattr_delete_fd(tmpfd, uninit, "foo");
 
    char* badbuff = malloc(64);
    free(badbuff);
    (void)extattr_get_file("test1", EXTATTR_NAMESPACE_USER, "bar", badbuff, 64);
    (void)extattr_get_link("test1", EXTATTR_NAMESPACE_USER, "bar", badbuff, 64);
    (void)extattr_get_fd(tmpfd, EXTATTR_NAMESPACE_USER, "bar", badbuff, 64);
    badbuff = malloc(64);
    free(badbuff);
    (void)extattr_list_file("test1", EXTATTR_NAMESPACE_USER, badbuff, 64);
    (void)extattr_list_link("test1", EXTATTR_NAMESPACE_USER, badbuff, 64);
    (void)extattr_list_fd(tmpfd, EXTATTR_NAMESPACE_USER, badbuff, 64);
 
    char* badstring = strdup("test2");
    free(badstring);
    (void)extattr_get_file(badstring, EXTATTR_NAMESPACE_USER, "bar", buff, sizeof(buff));
    (void)extattr_get_link(badstring, EXTATTR_NAMESPACE_USER, "bar", buff, sizeof(buff));
    (void)extattr_get_file("test1", EXTATTR_NAMESPACE_USER, badstring, buff, sizeof(buff));
    (void)extattr_get_link("test1", EXTATTR_NAMESPACE_USER, badstring, buff, sizeof(buff));
    (void)extattr_list_file(badstring, EXTATTR_NAMESPACE_USER, buff, sizeof(buff));
    (void)extattr_delete_file(badstring, EXTATTR_NAMESPACE_USER, "foo");
    (void)extattr_delete_link(badstring, EXTATTR_NAMESPACE_USER, "foo");
 
    int badfd = tmpfd;
    VALGRIND_MAKE_MEM_UNDEFINED(&badfd, sizeof(int));
    (void)extattr_get_fd(badfd, EXTATTR_NAMESPACE_USER, "bar", buff, sizeof(buff));
    (void)extattr_list_fd(badfd, EXTATTR_NAMESPACE_USER, buff, sizeof(buff));
    (void)extattr_delete_fd(badfd, EXTATTR_NAMESPACE_USER, "foo");

    int badsize = sizeof(buff);
    VALGRIND_MAKE_MEM_UNDEFINED(&badsize, sizeof(int));    
    extattr_get_file("test1", EXTATTR_NAMESPACE_USER, "bar", buff, badsize);
    extattr_get_link("test1", EXTATTR_NAMESPACE_USER, "bar", buff, badsize);
    extattr_get_fd(tmpfd, EXTATTR_NAMESPACE_USER, "bar", buff, badsize);
    extattr_list_file("test1", EXTATTR_NAMESPACE_USER, buff, badsize);
    extattr_list_link("test1", EXTATTR_NAMESPACE_USER, buff, badsize);
    extattr_list_fd(tmpfd, EXTATTR_NAMESPACE_USER, buff, badsize);

    close (tmpfd);
    unlink(tmpfile);
}

