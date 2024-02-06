#include <openssl/crypto.h>                                                                                                                                       

int main(void)
{
    CRYPTO_secure_malloc_init(1<<20, 8);
    CRYPTO_secure_malloc_done();
    return 0;
}
