#include "strlib.h"

#include <stdlib.h>
#include <string.h>

char * copyStr( const char * str )
{
  char * p = malloc( sizeof(char) * (strlen(str) + 1) );
  strcpy( p, str );

  return p;
}
