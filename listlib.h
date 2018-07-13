#ifndef LISTLIB_H
#define LISTLIB_H

#include "maindefs.h"

typedef void (*t_destructor)(void *);

struct list
{
    void ** xs;
    unsigned int size;
    unsigned int allocated;
};

struct list * allocList( void );
void initList( struct list * l );
void freeList( struct list * l, t_destructor );
void freeListExcl( struct list * l, t_destructor );

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

int findElem(
    struct list * l,
    void * e,
    bool (*cmpElems)(void *,
                     void *) );

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
void freeMap( struct map * m, t_destructor dKeys, t_destructor dVals );

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
