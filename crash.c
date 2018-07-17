#include "crash.h"
#include "strlib.h"
#include "curses_ctrl.h"

#include <stdlib.h>

void crash( const char * reason )
{
  if ( isCursesEnabled() ) exitCurses( true );

  const char * crashReason = concatStrs( "Crash! Reason: ", reason );
  if ( crashReason != NULL )
  {
    perror( crashReason );
  }

  exit( EXIT_FAILURE );
}
