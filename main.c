#include "curses_ctrl.h"
#include "sheet.h"
#include "listlib_test.h"

#include <stdlib.h>
#include "strlib.h"

#include "ghci.h"
#include "debug.h"


void exitSignal( int k )
{
  exitCurses( false );
}

void atExitProg( void )
{
  if ( isCursesEnabled() )
  {
    exitSheet();
    exitCurses( true );
  }
}

void identity( void * p )
{
}

int main( int argc, char ** argv )
{
  atexit( atExitProg );

  init_debug();
  dump_txt( "initializing ghci backend\n" );
  init_ghci();

  dump_txt( "initializing ncurses\n" );
  initCurses();
  dump_txt( "initializing spreadsheet\n" );
  initSheet();

  dump_txt( "everything initialized, entering control loop\n" );
  cursesCtrlLoop();

  exitSheet();
  exitCurses( true );
  exit_ghci();

  dump_txt( "shutdown\n" );

  exit_debug();

  return 0;
}
