
#include <stdio.h>
#include <stdlib.h>

char* p;

void ddd  ( void ) { p[-1] += 'z'; }
void ccc  ( void ) { ddd(); }
void bbb  ( void ) { ccc(); }
void aaa  ( void ) { bbb(); }

void zzzzzzz  ( void ) { p = malloc(10); }
void yyy  ( void ) { zzzzzzz(); }
void xxx  ( void ) { yyy(); }
void www  ( void ) { xxx(); }

int main ( void ) { www(); aaa(); return 0; }
