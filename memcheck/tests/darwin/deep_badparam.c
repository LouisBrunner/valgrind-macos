#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

int func_six(int x)
{
    char b[32];
    int r = write(1, b, sizeof(b));
    return x;
}

int func_five(int x)
{
    return func_six(x + 5);
}

int func_four(int x)
{
    return func_five(x + 4);
}

int func_three(int x)
{
    return func_four(x + 3);
}

int func_two(int x)
{
    return func_three(x + 2);
}

int func_one(int x)
{
    return func_two(x + 1);
}

int main(void)
{
    func_one(10);
    return 0;
}
