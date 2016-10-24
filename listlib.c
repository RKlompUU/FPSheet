#include "listlib.h"
#include "safety.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "crash.h"

#define ALLOC_BLOCK_SIZE 50

struct list * allocList( void )
{
    struct list * l = malloc( sizeof(struct list) );
    CHECK_ALLOC( l );

    initList( l );
    return l;
}

void initList( struct list * l )
{
    unsigned int initSize = ALLOC_BLOCK_SIZE;

    l->xs = malloc( sizeof(void *) * initSize );
    l->size = 0;
    l->allocated = initSize;

    CHECK_ALLOC( l->xs );
}

void freeListExcl( struct list * l )
{
    for ( unsigned int i = 0; i < l->size; i++ )
        free( l->xs[i] );

    free( l->xs );
}
void freeList( struct list * l )
{
    freeListExcl( l );
    free( l );
}

void increaseList( struct list * l )
{
    l->allocated += ALLOC_BLOCK_SIZE;

    l->xs = realloc( l->xs, sizeof(void *) * l->allocated );
    CHECK_ALLOC( l->xs );
}

void pushBack( struct list * l,
               void * x )
{
    while ( l->size >= l->allocated )
        increaseList( l );

    l->xs[l->size++] = x;
}

void destroy( struct list * l,
              unsigned int i )
{
    free( l->xs[i] );
    memmove( &l->xs[i], &l->xs[i + 1], sizeof(void *) * (l->size - (i + 1)) );
    l->size--;
}

void * get( struct list * l,
            unsigned int i )
{
#ifdef PARAM_CHECKS
    if( i >= l->size )
    {
        crash( "get out of bounds!" );
    }
#endif

    return l->xs[i];
}

int getI( struct list * l,
          unsigned int i )
{
    return *((int *) get( l, i ));
}

char getC( struct list * l,
           uint i )
{
    return *((char *) get( l, i ));
}

struct map * allocMap( int (*cmpKeys)( void *,
                                       void * ) )
{
    struct map * m = malloc( sizeof(struct map) );
    CHECK_ALLOC( m );

    m->keys = allocList();
    m->vals = allocList();
    m->cmpKeys = cmpKeys;
    m->pSize = &m->vals->size;

    return m;
}

void freeMap( struct map * m )
{
    freeList( m->keys );
    freeList( m->vals );

    free( m );
}

void * getVal( struct map * m,
               unsigned int i )
{
    if( i >= *m->pSize )
        return NULL;

    return get( m->vals, i );
}

void * getKey( struct map * m,
               unsigned int i )
{
    if( i >= *m->pSize )
        return NULL;

    return get( m->keys, i );
}

void mapAdd( struct map * m,
             void * k,
             void * v )
{
    pushBack( m->keys, k );
    pushBack( m->vals, v );
}

void * mapFind( struct map * m,
                void * k )
{
    for ( unsigned int i = 0; i < *m->pSize; i++ )
    {
        if ( (*m->cmpKeys)( get( m->keys, i ), k ) == 0 )
            return get( m->vals, i );
    }

    return NULL;
}
