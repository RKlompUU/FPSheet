#include "curses_ctrl.h"
#include "sheet.h"
#include "listlib_test.h"

#include <stdlib.h>
#include "strlib.h"

#include "ghci.h"
#include "debug.h"

void exitSignal( int k )
{
    exitCurses( true );
}

void atExitProg( void )
{
    if ( isCursesEnabled() )
    {
        exitCurses( true );
        exitSheet();
    }
}

int main( int argc,
          char ** argv )
{
    atexit( atExitProg );

    init_debug();
    init_ghci();

    //testIntList();
    initCurses();
    initSheet();

    // TODO: change this to escape: http://stackoverflow.com/questions/5977395/ncurses-and-esc-alt-keys
    subKey( KEY_END, exitSignal );

    cursesCtrlLoop();
    exitCurses( true );
    exitSheet();

    exit_debug();

    return 0;
}
