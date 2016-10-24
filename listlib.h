#ifndef LISTLIB_H
#define LISTLIB_H

#include "maindefs.h"

struct list
{
    void ** xs;
    unsigned int size;
    unsigned int allocated;
};

struct list * allocList( void );
void initList( struct list * l );
void freeList( struct list * l );
void freeListExcl( struct list * l );

void pushBack( struct list * l,
               void * x );
void destroy( struct list * l,
              unsigned int i );
void * get( struct list * l,
            unsigned int i );

int getI( struct list * l,
          unsigned int i );
char getC( struct list * l,
           unsigned int i );

struct map
{
    struct list * keys;
    struct list * vals;

    unsigned int * pSize;

    /*
     * Return:
     *   | k1 < k2  = -1
     *   | k1 == k2 = 0
     *   | k1 > k2  = 1
     */
    int (*cmpKeys)( void *,
                    void * );
};

struct map * allocMap( int (*cmpKeys)( void *,
                                       void * ) );
void freeMap( struct map * m );

void mapAdd( struct map * m,
             void * k,
             void * v );
void * mapFind( struct map * m,
                void * k );

void * getKey( struct map * m,
               uint i );
void * getVal( struct map * m,
               uint i );

#endif
