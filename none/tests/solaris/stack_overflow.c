/* There should be a user message about the overflow.
   Written in a single line so there is no confusion on what line
   the overflow occurs. */

int main(int argc, char *argv[]) { volatile int arr[1000] = {0}; return main(arr[argc%2], 0); }
