#include "memlib.h"
#include "safety.h"


#include <stdlib.h>

int * newI( int x )
{
  int * p = malloc( sizeof(int) );
  CHECK_ALLOC( p );
  *p = x;
  return p;
}

uint * newUI( uint x )
{
    uint * p = malloc( sizeof(uint) );
    CHECK_ALLOC( p );
    *p = x;
    return p;
}
