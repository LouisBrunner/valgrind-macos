#include <stdlib.h>

struct instance
{
    unsigned myVal:1;
};

static struct instance* myInstance;

int main(int argc, char** argv)
{
    float g = 1.0f;
    
    myInstance = malloc(sizeof(struct instance));

    myInstance->myVal = 1;

    if (g == 1.0f)
        return 0;
    else
        return 1;
}
