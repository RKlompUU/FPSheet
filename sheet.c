#include "sheet.h"
#include "strlib.h"

#include <stdlib.h>

#include <curses.h>

#include "curses_ctrl.h"


struct sheet s;

//
// Utility functions
//

int posCmp( void * p1_, void * p2_ )
{
  struct pos * p1 = (struct pos *)p1_;
  struct pos * p2 = (struct pos *)p2_;

  if( p1->row < p2->row )
    return -1;
  if( p1->row > p2->row )
    return 1;

  if( p1->col < p2->col )
    return -1;
  if( p1->col > p2->col )
    return 1;

  return 0;
}

void gotoOff( int r, int c )
{
  if( r < 0 || c < 0 )
    return;

  s.rowOff = r;
  s.colOff = c;

  s.draw = true;
}

void moveCur( int r, int c )
{
  if( r < 0 || c < 0 )
    return;

  s.curRow = r;
  s.curCol = c;

  s.draw = true;

  if( r < s.rowOff )
    gotoOff( r, s.colOff );
  int d = r - (int)s.lastR;
  if( d > 0 )
    gotoOff( s.rowOff + d, s.colOff );

  if( c < s.colOff )
    gotoOff( s.rowOff, c );
  d = c - (int)s.lastC;
  if( d > 0 )
    gotoOff( s.rowOff, s.colOff + d );
}

void sheetAction( int k )
{
  switch( k )
  {
  case KEY_UP:
    moveCur( s.curRow-1, s.curCol );
    break;
  case KEY_DOWN:
    moveCur( s.curRow+1, s.curCol );
    break;
  case KEY_LEFT:
    moveCur( s.curRow, s.curCol-1 );
    break;
  case KEY_RIGHT:
    moveCur( s.curRow, s.curCol+1 );
    break;
  }
}

void initSheet( void )
{
  s.cells = allocMap( posCmp );

  s.rowOff = 0;
  s.colOff = 0;

  int h, w;
  getmaxyx( stdscr, h, w );
  s.wH = (uint) h;
  s.wW = (uint) w;

  s.hW = 2;
  s.hH = 2;

  s.cW = 10;
  s.cH = 1;

  s.fH = 1;
  s.fW = s.wW;

  s.curRow = 0;
  s.curCol = 0;

  s.draw = true;

  subKey( KEY_UP, sheetAction );
  subKey( KEY_DOWN, sheetAction );
  subKey( KEY_LEFT, sheetAction );
  subKey( KEY_RIGHT, sheetAction );
}

void exitSheet( void )
{
  freeMap( s.cells );
}


struct cell * newC( const char * txt, struct pos * p )
{
  struct cell * c = malloc( sizeof(struct cell) );
  c->txt = copyStr( txt );
  c->uFlag = true;
  c->p = p;

  return c;
}

struct cell * findCellP( struct map * cells, struct pos p )
{
  return (struct cell *) mapFind( cells, &p );
}
struct cell * findCellP2( struct map * cells, unsigned int row, unsigned int col )
{
  struct pos p;
  p.row = row;
  p.col = col;

  return (struct cell *) mapFind( cells, &p );
}

struct cell * getCellP( struct list * l, unsigned int i )
{
  return (struct cell *)get( l, i );
}

struct pos getPos( struct list * l, unsigned int i )
{
  return *(struct pos *)get( l, i );
}
