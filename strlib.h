#ifndef STRLIB_H
#define STRLIB_H

#include "listlib.h"
#include "maindefs.h"

#include <stdbool.h>
#include <stddef.h> // size_t

char * copyStr( const char * str );
char * concatStrs( const char * str1,
                   const char * str2 );

void appendChar( char ** str,
                 const char c );

char * iStr( int i );
char * uiStr( uint i );

luint iLength( int i );
luint uiLength( uint i );

char * uint2Alpha( uint i );
uint alpha2Uint( const char * str );

char * list2Str( struct list * l );

char * curPos2Str( uint r,
                   uint c );

void refStr2Pos(
    const char * str,
    size_t strLen,
    uint * r,
    uint * c );
void revPosStr( char * str, size_t strLen );

bool iswordPosRef( char * str );

uint wordLength( const char * str );

size_t sizeofUIntStr( uint x );

#endif
