#include <stdlib.h>

int main() {
	char *p = malloc(1);
        p[-1] = 0;
        return 0;
}
