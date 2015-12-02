#include "curses_ctrl.h"
#include "listlib.h"
#include "safety.h"

#include "strlib.h"

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

  initSheet();
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

void drawSheet()
{
  drawHeaders();
}

void drawCell( const struct cell * const c )
{
}

void drawHeaders( void )
{
  // Line numbers
  uint bcH = s.cH + 1; // Bordered cell height
  uint lastN = (uint)s.rowOff + (s.wH - s.hH - bcH - 1)/bcH; // - bcH, - 1 to prevent a possible half-row
  // This conversion from luint to uint is save:
  //    The length of row numbers will never even come close to the limits of uint
  //    , since long before that point would've been reached other limits would've been reached already
  s.hW = (uint) uiLength( lastN ) + 1; // + 1 for the border

  for( uint n = (uint) s.rowOff; n <= lastN ; n++ )
  {
    uint r = s.hH + (n-(uint)s.rowOff)*bcH + bcH/2;
    char * str = uiStr( n );
    mvaddstr( (int)r, 0, str ); // Save conversion
    free( str ); // TODO: can this be done?
  }
  // Line numbers border
  for( uint r = s.hH; r < s.wH; r++ )
  {
    mvaddch( (int)r, (int)s.hW-1, '|' );
  }

  refresh();
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
