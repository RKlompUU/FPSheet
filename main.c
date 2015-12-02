#include "curses_ctrl.h"
#include "listlib_test.h"

#include <stdlib.h>

void exitSignal( int k )
{
  exitCurses( true );
}

void atExitProg( void )
{
  if( isCursesEnabled() )
    exitCurses( true );
}

int main( int argc, char ** argv )
{
  atexit( atExitProg );

  //testIntList();
  initCurses();

  subKey( KEY_END, exitSignal );

  cursesCtrlLoop();

  return 0;
}
