
typedef unsigned short UShort;

UShort mul16 ( UShort a, UShort b );

int main ( int argc, char** argv )
{
   UShort x = mul16 ( 10, 20 );
   return ((int)x) - 200;
}

UShort mul16 ( UShort a, UShort b )
{
   return a * b;
}
