#ifndef STRLIB_H
#define STRLIB_H

#include "listlib.h"
#include "maindefs.h"

#include <stddef.h> // size_t

char * copyStr( const char * str );
char * concatStrs( const char * str1, const char * str2 );

char * appendChar( char * str, const char c );

char * iStr( int i );
char * uiStr( uint i );

luint iLength( int i );
luint uiLength( uint i );

char * uint2Alpha( uint i );

char * list2Str( struct list * l );

char * curPos2Str( uint r, uint c );

size_t sizeofUIntStr( uint x );

#endif
