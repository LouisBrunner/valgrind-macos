
int main ( void )
{
   char* p = (char*)0xE000000;
   return ((int(*)(void)) p) ();
}
