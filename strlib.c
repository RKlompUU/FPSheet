#include "strlib.h"

#include <stdlib.h>
#include <string.h>

#include <stdio.h>

#include "mathlib.h"
#include "listlib.h"

#define DEFAULT_STR_SIZE 80

#define ALPHA_SIZE 26 // 'Z' - 'A'
#define ALPHA_START 'A'

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

char * uint2Alpha( uint i )
{
  struct list strBuilder;
  initList( &strBuilder );

  do
  {
    i -= 1;
    char * c = malloc( sizeof(char) );
    *c = (char) ((uint)ALPHA_START + (i % ALPHA_SIZE));
    pushBack( &strBuilder, c );
    i /= ALPHA_SIZE;
  } while( i > 0 );

  char * str = list2Str( &strBuilder );
  freeListExcl( &strBuilder );

  return str;
}

char * list2Str( struct list * l )
{
  uint length;
  if( getC( l, l->size-1 ) == '\0' )
    length = l->size;
  else
    length = l->size + 1;

  char * s = malloc( sizeof(char) * length );
  for( uint i = 0; i < l->size; i++ )
  {
    s[i] = getC( l, i );
  }
  s[length-1] = '\0';

  return s;
}

char * uint2Alpha_( uint i )
{
  uint l;
  if( i <= 1 )
    l = 1;
  else
    l = logn( ALPHA_SIZE, i-1 ) + 1;
  char * s = malloc( sizeof(char) * l + 1 );
  for( uint n = 0; n < l; n++ )
  {
    i -= 1;
    s[n] = (char) ((uint)ALPHA_START + (i % ALPHA_SIZE));
    i /= ALPHA_SIZE;
  }
  s[l] = '\0';

  return s;
}
