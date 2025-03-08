#include <fcntl.h>

int main(void) {
        fcntl(-1, F_GETFD);
        return 0;
}

