#include "strlib.h"

#include <stdlib.h>
#include <string.h>

#include <stdio.h>

#define DEFAULT_STR_SIZE 80

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

char * uiStr( uint i )
{
  char * s = malloc( sizeof(char) * DEFAULT_STR_SIZE );
  sprintf( s, "%u", i );

  return s;
}

char * iStr( int i )
{
  char * s = malloc( sizeof(char) * DEFAULT_STR_SIZE );
  sprintf( s, "%d", i );

  return s;
}

luint iLength( int i )
{
  char * s = iStr( i );
  luint l = strlen( s );
  free( s );

  return l;
}

luint uiLength( uint i )
{
  char * s = uiStr( i );
  luint l = strlen( s );
  free( s );

  return l;
}
