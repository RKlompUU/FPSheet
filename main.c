#include "curses_ctrl.h"
#include "sheet.h"
#include "listlib_test.h"

#include <stdlib.h>

void exitSignal( int k )
{
  exitCurses( true );
}

void atExitProg( void )
{
  if( isCursesEnabled() )
  {
    exitCurses( true );
    exitSheet();
  }
}

void dH( int k )
{
  drawHeaders();
}

int main( int argc, char ** argv )
{
  atexit( atExitProg );

  //testIntList();
  initCurses();
  initSheet();

  subKey( KEY_END, exitSignal );
  subKey( 'h', dH );

  cursesCtrlLoop();
  exitSheet();

  return 0;
}
