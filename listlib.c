#include "listlib.h"
#include "safety.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#define ALLOC_BLOCK_SIZE 50


struct list * allocList( void )
{
  unsigned int initSize = ALLOC_BLOCK_SIZE;

  struct list * l = malloc( sizeof(struct list) );
  CHECK_ALLOC( l );

  l->xs = malloc( sizeof(void *) * initSize );
  l->size = 0;
  l->allocated = initSize;

  CHECK_ALLOC( l->xs );

  return l;
}

void freeList( struct list * l )
{
  for( unsigned int i = 0; i < l->size; i++ )
    free( l->xs[i] );

  free( l->xs );
  free( l );
}

void increaseList( struct list * l )
{
  l->allocated += ALLOC_BLOCK_SIZE;

  l->xs = realloc( l->xs, sizeof(void *) * l->allocated );
  CHECK_ALLOC( l->xs );
}

void push_back( struct list * l, void * x )
{
  while( l->size >= l->allocated )
    increaseList( l );

  l->xs[l->size++] = x;
}

void * get( struct list * l, unsigned int i )
{
#ifdef PARAM_CHECKS
  if( i >= l->size )
  {
    perror( "get out of bounds!" );
    exit( EXIT_FAILURE );
  }
#endif

  return l->xs[i];
}

int getI( struct list * l, unsigned int i )
{
  return *((int *) get( l, i ));
}
