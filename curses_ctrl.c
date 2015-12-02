#include "curses_ctrl.h"

#include <ncurses.h>

void initCurses( void )
{
  initscr();
  raw();
  keypad( stdscr, TRUE );
  noecho();
}

void exitCurses( void )
{
  endwin();
}

void render( const struct sheet * s )
{
  for( unsigned int i = 0; i < *s->cells->pSize; i++ )
  {
    struct cell * c = getCellP( s->cells->vals, i );
    if( c->uFlag )
    {
      drawCell( s, c );
    }
  }
}


void drawCell( const struct sheet * const s, const struct cell * const c )
{
}

void drawHeaders( struct sheet * s )
{

}

