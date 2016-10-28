#include "curses_ctrl.h"
#include "sheet.h"
#include "listlib_test.h"

#include <stdlib.h>
#include "strlib.h"

#include "ghci.h"
#include "debug.h"

#include "sheetParser.h"

void exitSignal( int k )
{
    exitCurses( false );
}

void atExitProg( void )
{
    if ( isCursesEnabled() )
    {
        //exitSheet();
        //exitCurses( true );
    }
}

int main( int argc,
          char ** argv )
{
    atexit( atExitProg );

    init_debug();
    dump_txt( "initializing ghci backend\n" );
    init_ghci();

    dump_txt( "initializing ncurses\n" );
    initCurses();
    dump_txt( "initializing spreadsheet\n" );
    initSheet();

    // TODO: change this to escape: http://stackoverflow.com/questions/5977395/ncurses-and-esc-alt-keys
    subKey( KEY_END, exitSignal );

    dump_txt( "everything initialized, entering control loop\n" );
    cursesCtrlLoop();
    exitCurses( true );
    exitSheet();

    dump_txt( "test" );

    exit_debug();

    return 0;
}
