// Example copied from https://en.cppreference.com/w/c/string/byte/memccpy
#include <ctype.h>
#include <stdio.h>
#include <string.h>
 
int main(void)
{
    const char src0[] = "Stars: Altair, Sun, Vega.";
    const char* src = strdup(src0);
    const char terminal[] = {':', ' ', ',', '.', '!'};
    char dest[sizeof src0];
    const char alt = '@';
 
    for (size_t i = 0; i != sizeof terminal; ++i)
    {
        void* to = memccpy(dest, src, terminal[i], sizeof dest);
 
        printf("Terminal '%c' (%s):\t\"", terminal[i], to ? "found" : "absent");
 
        // if `terminal` character was not found - print the whole `dest`
        to = to ? to : dest + sizeof dest;
 
        for (char* from = dest; from != to; ++from)
            putchar(isprint(*from) ? *from : alt);
 
        puts("\"");
    }
 
 
    puts("\n" "Separate star names from distances (ly):");
    const char *star_distance[] = {
        "Arcturus : 37", "Vega : 25", "Capella : 43", "Rigel : 860", "Procyon : 11"
    };
    char names_only[64];
    char *first = names_only;
    char *last = names_only + sizeof names_only;
 
    for (size_t t = 0; t != (sizeof star_distance) / (sizeof star_distance[0]); ++t)
    {
        if (first)
            first = memccpy(first, star_distance[t], ' ', last - first);
        else
            break;
    }
 
    if (first)
    {
        *first = '\0';
        puts(names_only);
    }
    else
        puts("Buffer is too small.");
}

