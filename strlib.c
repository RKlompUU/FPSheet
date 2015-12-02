#include "strlib.h"

#include <stdlib.h>
#include <string.h>

char * copyStr( const char * str )
{
  char * p = malloc( sizeof(char) * (strlen(str) + 1) );
  strcpy( p, str );

  return p;
}

char * concatStrs( const char * str1, const char * str2 )
{
  char * p = malloc( sizeof(char) * (strlen(str1) + strlen(str2) + 1));
  strcpy( p, str1 );
  strcpy( p + sizeof(char) * strlen(str1), str2 );

  return p;
}
