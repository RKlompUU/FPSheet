#include "listlib_test.h"

#include "listlib.h"
#include "memlib.h"

#include <stdlib.h>
#include <stdio.h>

void testIntList( void )
{
    struct list * l = allocList();

    for ( int i = 0; i < 50; i++ )
        pushBack( l, newI( i ) );

    printf( "List size: %u\nAllocated: %u\n", l->size, l->allocated );
    pushBack( l, newI( 0 ) );
    printf( "List size: %u\nAllocated: %u\n", l->size, l->allocated );

    for ( unsigned int i = 0; i < l->size; i++ )
        printf( "l->xs[i]: %i\n", getI( l, i ) );

    freeList( l, free );
}
