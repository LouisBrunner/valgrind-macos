
void bar ( void );

int main ( int argc, char** argv )
{
  int i;
  for (i = 0; i < argc; i++) 
    bar();
  return 0;
}

void bar ( void ) { }
