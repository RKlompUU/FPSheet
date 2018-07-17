#include "debug.h"

#include <stdio.h>
#include "safety.h"
#include <string.h>

static const char fpath[80] = "debug_output.txt";
static FILE* f;

void init_debug( void )
{
  remove( fpath );
  f = fopen( fpath, "a" );
}
void exit_debug( void )
{
  fclose( f );
}

void dump_txt( const char * txt )
{
  fwrite( txt, sizeof(char), strlen(txt), f );
  fflush( f );
}

FILE * dumpFile( void )
{
  return f;
}
