// syslog() needs a suppression on Mac OS X (bug 191192).  This tests that.

#include <syslog.h>

int main()
{
    syslog(LOG_USER|LOG_DEBUG, "valgrind/none/tests/syslog: test message");
    return 0;
}
