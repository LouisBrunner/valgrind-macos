#include <new>

// At one point, Valgrind wasn't overriding these 'nothrow' versions;  since
// they call malloc(), the calls to 'delete' caused bogus mismatch errors.

int main()
{
    int * a = new (std::nothrow) int;
    int * b = new (std::nothrow) int[5];
    delete    a;
    delete [] b;
}

