#ifndef DEBUG_H
#define DEBUG_H

#include <stdio.h>

void init_debug( void );
void exit_debug( void );
void dump_txt( const char * txt );

FILE* start_dump( void );
void end_dump( FILE* f );

#endif
