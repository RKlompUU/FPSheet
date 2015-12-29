#ifndef STRLIB_H
#define STRLIB_H

#include "maindefs.h"

char * copyStr( const char * str );
char * concatStrs( const char * str1, const char * str2 );

char * iStr( int i );
char * uiStr( uint i );

luint iLength( int i );
luint uiLength( uint i );

char * uint2Alpha( uint i );


#endif
