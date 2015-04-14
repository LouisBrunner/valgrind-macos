int main ( void )
{
   *(volatile char *)0xDEADBEEF = 'x'; 
   return 0;
}
