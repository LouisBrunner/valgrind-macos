
#include <string.h>

int main() {
        char buffer[200] = "HALLO";
        char *b2 = strdup(strcat(buffer, "THis is a very long strings"));
        int len = strlen(b2);
        if (len < 12)
                return 0;
        else
                return 1;
}

