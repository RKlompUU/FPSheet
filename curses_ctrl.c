#include "curses_ctrl.h"
#include "listlib.h"
#include "safety.h"

#include <ncurses.h>


struct sheet s;

struct keyListener
{
  int k;

  void (*callback)( int );
};

struct list kListeners;
struct keyListener * getListener( unsigned int i )
{
  return (struct keyListener *)get( &kListeners, i );
}

bool cursesEnabled = false;
bool isCursesEnabled( void )
{
  return cursesEnabled;
}

void initCurses( void )
{
  initscr();
  raw();
  keypad( stdscr, TRUE ); // Get function keys, arrow keys etc.
  cbreak(); // Instantly send keypress events to this application, don't wait for EOLs
  noecho();
  nonl();
  cursesEnabled = true;
}

void exitCurses( bool cleanupMem )
{
  cursesEnabled = false;
  endwin();

  if( cleanupMem )
  {
    freeListExcl( &kListeners );
  }
}

void cursesCtrlLoop( void )
{
  while( cursesEnabled )
  {
    int k = getch();

    mvprintw(0, 0, "Key: %u", k);
    refresh();

    handleEvent( k );
  }
}

void render( void )
{
  for( unsigned int i = 0; i < *s.cells->pSize; i++ )
  {
    struct cell * c = getCellP( s.cells->vals, i );
    if( c->uFlag )
    {
      drawCell( c );
    }
  }
}


void drawCell( const struct cell * const c )
{
}

void drawHeaders( void )
{

}

void handleEvent( int k )
{
  for( unsigned int i = 0; i < kListeners.size; i++ )
  {
    struct keyListener * l = getListener( i );
    if( k == l->k )
      (*l->callback)( k );
  }
}

void subKey( int k, void (*callback)(int) )
{
  struct keyListener * l = malloc( sizeof(struct keyListener) );
  CHECK_ALLOC( l );

  l->k = k;
  l->callback = callback;

  pushBack( &kListeners, l );
}

void unsubKey( int k, void (*callback)(int) )
{
  for( unsigned int i = 0; i < kListeners.size; i++ )
  {
    struct keyListener * l = getListener( i );
    if( l->k == k && l->callback == callback )
      destroy( &kListeners, i-- );
  }
}
