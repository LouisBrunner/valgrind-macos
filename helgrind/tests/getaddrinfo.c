#include <netdb.h>
#include <pthread.h>
#include <sys/socket.h>
#include <sys/types.h>

struct data {
    const char *hostname;
    struct addrinfo *res;
};

struct data threaddat[5] = {
    { "www.freebsd.org", 0 },
    { "www.google.com", 0 },
    { "www.freshports.org", 0 },
    { "www.github.com", 0 },
    { "www.kernel.org", 0 }
};

pthread_t threads[5];

void *resolve(void *d) {
    struct data *data = d;
    getaddrinfo(data->hostname, 0, 0, &data->res);
    return 0;
}

int main(void) {
    int i;
    for (i = 0; i < 5; ++i) {
        pthread_create(&threads[i], 0, resolve, &threaddat[i]);
    }
    for (i = 0; i < 5; ++i) {
        pthread_join(threads[i], 0);
        freeaddrinfo(threaddat[i].res);
    }
    return 0;
}

