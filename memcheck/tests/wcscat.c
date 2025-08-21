// See https://bugs.kde.org/show_bug.cgi?id=501893
#include <wchar.h>
#include <stdio.h>
#include <locale.h>
#include <stdlib.h>

int main(void)
{
    wchar_t* str = malloc(sizeof(L"/usr/lib/python310.zip:/usr/lib/python3.10:"));
    wchar_t* add1 = wcsdup(L"/usr/lib/python310.zip:/usr/lib/python3.10");
    wchar_t* add2 = wcsdup(L":");
    str[0] = 0;
    wcscat(str, add1);
    wcscat(str, add2);
    setlocale(LC_ALL, "en_US.utf8");
    printf("%ls\n", str);
    free(str);
    free(add1);
    free(add2);
}
