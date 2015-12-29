#include "curses_ctrl.h"
#include "listlib.h"
#include "safety.h"

#include "strlib.h"

#include <ncurses.h>


// struct sheet s; < extern

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

  initList( &kListeners );

  cursesEnabled = true;
}

void exitCurses( bool cleanupMem )
{
  if( cursesEnabled )
  {
    if( cleanupMem )
    {
      freeListExcl( &kListeners );
    }

    endwin();
  }

  cursesEnabled = false;
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

void cleanArea( uint x1, uint y1, uint x2, uint y2 )
{
  for( uint x = x1; x < x2; x++ )
  {
    for( uint y = y1; y < y2; y++ )
    {
      mvaddch( (int)x, (int)y, ' ' );
    }
  }
}

void drawHeaders( void )
{
  uint oldHW = s.hW;


  //
  // Row numbers
  //

  uint bcH = s.cH + 1; // Bordered cell height
  uint lastN = (uint)s.rowOff + (s.wH - s.hH - bcH - 1)/bcH; // - bcH, - 1 to prevent a possible half-row
  // This conversion from luint to uint is save:
  //    The length of row numbers will never even come close to the limits of uint
  //    , since long before that point other limits would've been reached already
  s.hW = (uint) uiLength( lastN ) + 1; // + 1 for the border

  cleanArea( s.hH, 0, s.wH, oldHW );

  for( uint n = (uint) s.rowOff; n <= lastN; n++ )
  {
    uint r = s.hH + (n-(uint)s.rowOff)*bcH + bcH/2;
    char * str = uiStr( n );
    mvaddstr( (int)r, 0, str ); // Save conversion
    free( str );
  }
  // Row numbers border
  for( uint r = s.hH; r < s.wH; r++ )
  {
    mvaddch( (int)r, (int)s.hW-1, '|' );
  }


  //
  // Column letters
  //

  uint bcW = s.cW + 1;
  uint lastC = (uint)s.colOff + (s.wW - s.hW - bcW - 1)/bcW;

  cleanArea( 0, 0, s.hH, s.wW );

  for( uint n = (uint) s.colOff; n <= lastC; n++ )
  {
    uint c = s.hW + (n-(uint)s.colOff)*bcW + bcW/2;
    char * str = uint2Alpha( n+1 );
    mvaddstr( 0, (int) c, str ); // Save conversion
    free( str );
  }

  for( uint c = s.hW; c < s.wW; c++ )
  {
    mvaddch( (int)s.hH-1, (int)c, '-' );
  }
  mvaddch( (int)s.hH-1, (int)s.hW-1, ',' );
  mvaddch( (int)s.wH-1, (int)s.wW-1, ' ' );

  refresh();
}

void handleEvent( int k )
{
  for( unsigned int i = 0; i < kListeners.size && cursesEnabled; i++ )
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
