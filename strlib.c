#include "strlib.h"

#include <stdlib.h>
#include <string.h>

#include <stdio.h>

#include "maindefs.h"

#include "mathlib.h"
#include "listlib.h"
#include "safety.h"

#define DEFAULT_STR_SIZE 80

#define ALPHA_SIZE 26 // 'Z' - 'A'
#define ALPHA_START 'A'

char * copyStr( const char * str )
{
  char * p = malloc( sizeof(char) * (strlen( str ) + 1) );
  CHECK_ALLOC( p )
    strcpy( p, str );

  return p;
}

char * concatStrs( const char * str1, const char * str2 )
{
  char * p = malloc( sizeof(char) * (strlen( str1 ) + strlen( str2 ) + 1) );
  CHECK_ALLOC( p )
    strcpy( p, str1 );
  strcpy( p + sizeof(char) * strlen( str1 ), str2 );

  return p;
}

void appendChar( char ** str, const char c )
{
  long unsigned int newSize;
  if ( *str )
  {
    newSize = strlen( *str ) + 2; // + 1 for '\0' and + 1 for c
  }
  else
  {
    newSize = 2;
  }
  char * p = realloc( *str, sizeof(char) * newSize );
  p[newSize - 2] = c;
  p[newSize - 1] = '\0';

  *str = p;
}

void insertChar( char ** str, const char c, uint i )
{
  long unsigned int newSize;
  if ( *str )
  {
    newSize = strlen( *str ) + 2; // + 1 for '\0' and + 1 for c
  }
  else
  {
    newSize = 2;
  }
  char * p = realloc( *str, sizeof(char) * newSize );

  if( i > newSize - 2 )
    i = newSize - 2;
  else
  {
    // 1234
    //   2
    //
    // newsize: 6
    // i: 2
    //
    memmove( &p[i+1], &p[i], newSize - i - 1 );
  }
  p[i] = c;

  *str = p;
}



void removeChar( char ** str, uint i )
{
  memmove( &(*str)[i], &(*str)[i+1], strlen(*str) - i );
}

char * uiStr( uint i )
{
  char * s = malloc( sizeof(char) * DEFAULT_STR_SIZE );
  CHECK_ALLOC( s )
    sprintf( s, "%u", i );

  return s;
}

char * iStr( int i )
{
  char * s = malloc( sizeof(char) * DEFAULT_STR_SIZE );
  CHECK_ALLOC( s )
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
  i++; // This function is 1 based (i = 1 -> A)

  struct list strBuilder;
  initList( &strBuilder );

  do
  {
    i -= 1;
    char * c = malloc( sizeof(char) );
    CHECK_ALLOC( c )
      *c = (char) ((uint) ALPHA_START + (i % ALPHA_SIZE));
    pushBack( &strBuilder, c );
    i /= ALPHA_SIZE;
  }
  while ( i > 0 );

  char * str = list2Str( &strBuilder );
  freeListExcl( &strBuilder, free );

  return str;
}

uint alpha2Uint( const char * str )
{
  uint l = wordLength( str );
  uint res = 0;
  for( uint i = 0; i < l && str[i] != '\0'; i++ )
  {
    char c = str[i];
    if( c < 'A' || c > 'Z' )
      break;
    res += (c - 'A' + 1) * pow(ALPHA_SIZE, i);
  }
  return res - 1;
}

char * list2Str( struct list * l )
{
  uint length;
  if ( getC( l, l->size - 1 ) == '\0' )
    length = l->size;
  else
    length = l->size + 1;

  char * s = malloc( sizeof(char) * length );
  CHECK_ALLOC( s )
    for ( uint i = 0; i < l->size; i++ )
    {
      s[i] = getC( l, i );
    }
  s[length - 1] = '\0';

  return s;
}

char * curPos2Str( uint r, uint c )
{
  size_t t = sizeofUIntStr( r );
  char * rStr = malloc( t ); // sizeofUIntStr(r) );
  CHECK_ALLOC( rStr )
    sprintf( rStr, "%u", r );
  char * cStr = uint2Alpha( c );

  char * s = concatStrs( cStr, rStr );
  free( rStr );
  free( cStr );
  return s;
}

void refStr2Pos( const char * str, size_t strLen, uint * r, uint * c )
{
  uint rStart;
  for( rStart = 1;; rStart++ )
  {
    if( str[rStart] < 'A' || str[rStart] > 'Z' )
      break;
  }
  *c = alpha2Uint( str );
  *r = strtol( str + rStart, NULL, 10 );
  return;
}

size_t sizeofUIntStr( uint x )
{
  if ( x == 0 ) return 2;
  return (size_t) ((ceil( log10( x ) ) + 1) * sizeof(char)) + 1;
}

bool iswordPosRef( char * str )
{
  bool hasNum = false;
  bool hasChar = false;
  for( uint i = 0; i < strlen(str); i++ )
  {
    char c = str[i];
    if( !hasChar )
    {
      if( c >= 'A' && c <= 'Z' )
        hasChar = true;
      else
        return false;
    }
    else if( !hasNum )
    {
      if( c >= 'A' && c <= 'Z' )
        continue;
      if( c >= '0' && c <= '9' )
        hasNum = true;
      else
        return false;
    }
    else
    {
      if( c >= '0' && c <= '9' )
        continue;
      else if( c == ' ' )
        return true;
      return false;
    }
  }
  return hasNum && hasChar;
}

uint wordLength( const char * str )
{
  uint wl = 0;
  for( uint i = 0; i < strlen(str); i++ )
  {
    const char c = str[i];
    if( c == ' ' )
      return wl;
    wl++;
  }
  return wl;
}
