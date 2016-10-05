#include "sheet.h"
#include "strlib.h"

#include <stdlib.h>

#include <string.h>
#include <curses.h>

#include "curses_ctrl.h"

struct sheet s;

//
// Utility functions
//

int posCmp( void * p1_,
            void * p2_ )
{
    struct pos * p1 = (struct pos *) p1_;
    struct pos * p2 = (struct pos *) p2_;

    if ( p1->row < p2->row ) return -1;
    if ( p1->row > p2->row ) return 1;

    if ( p1->col < p2->col ) return -1;
    if ( p1->col > p2->col ) return 1;

    return 0;
}

void gotoOff( int r,
              int c )
{
    if ( r < 0 || c < 0 ) return;

    s.rowOff = r;
    s.colOff = c;

    s.draw = true;
}

void moveCur( int r,
              int c )
{
    if ( r < 0 || c < 0 ) return;

    s.curRow = r;
    s.curCol = c;

    s.draw = true;

    if ( r < s.rowOff ) gotoOff( r, s.colOff );
    int d = r - (int) s.lastR;
    if ( d > 0 ) gotoOff( s.rowOff + d, s.colOff );

    if ( c < s.colOff ) gotoOff( s.rowOff, c );
    d = c - (int) s.lastC;
    if ( d > 0 ) gotoOff( s.rowOff, s.colOff + d );
}

void editCell( int k )
{
    struct cell * c = findCellP2( s.cells, (uint) s.curRow, (uint) s.curCol );
    if ( c == NULL )
    {
        struct pos * p = malloc( sizeof(struct pos) );
        p->col = (uint) s.curCol;
        p->row = (uint) s.curRow;
        c = newC( p );
        c->txt = malloc( sizeof(char) );
        c->txt[0] = '\0';
    }
    switch( k )
    {
      case KEY_BACKSPACE:
      case 127:
      {
      long unsigned int length = strlen( c->txt );
      if( length > 0 )
      {
        c->txt[length-1] = '\0';
        c->uFlag = true;
        s.draw = true;
      }
      }
        break;
      default:
      appendChar( &c->txt, (char) k );
      c->uFlag = true;
      s.draw = true;
        break;
    }
}

void moveCursorKey( int k )
{
    switch ( k )
    {
        case KEY_UP:
        moveCur( s.curRow - 1, s.curCol );
            break;
        case KEY_DOWN:
        moveCur( s.curRow + 1, s.curCol );
            break;
        case KEY_LEFT:
        moveCur( s.curRow, s.curCol - 1 );
            break;
        case KEY_RIGHT:
        moveCur( s.curRow, s.curCol + 1 );
            break;
    }
}

void modeChange( int k )
{
    switch ( s.mode )
    {
        case MODE_NAVIG:
        switch ( k )
        {
            case KEY_ENTER:
            case '\r':
            case 'i':
            unsubGroup( GROUP_SUB_NAVIG, moveCursorKey );
            subGroup( GROUP_SUB_EDIT, editCell );

            s.mode = MODE_EDIT;
            s.draw = true;
                break;
        }
            break;
        case MODE_EDIT:
        switch ( k )
        {
            case KEY_ENTER:
            case '\r':
            unsubGroup( GROUP_SUB_EDIT, editCell );
            subGroup( GROUP_SUB_NAVIG, moveCursorKey );

            s.mode = MODE_NAVIG;
            s.draw = true;
                break;
        }
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

    s.prevRow = 0;
    s.prevCol = 0;

    s.draw = true;

    s.mode = MODE_NAVIG;

    addSubToGroup( KEY_UP, GROUP_SUB_NAVIG );
    addSubToGroup( KEY_DOWN, GROUP_SUB_NAVIG );
    addSubToGroup( KEY_LEFT, GROUP_SUB_NAVIG );
    addSubToGroup( KEY_RIGHT, GROUP_SUB_NAVIG );

    for ( int c = ' '; c <= '~'; c++ )
    {
        addSubToGroup( c, GROUP_SUB_EDIT );
    }
    addSubToGroup( KEY_BACKSPACE, GROUP_SUB_EDIT );
    addSubToGroup( 127, GROUP_SUB_EDIT );
    //
//    for ( int c = 'A'; c <= 'Z'; c++ )
//    {
//        addSubToGroup( c, GROUP_SUB_EDIT );
//    }

    subGroup( GROUP_SUB_NAVIG, moveCursorKey );

    subKey( KEY_ENTER, modeChange );
    subKey( '\r', modeChange );
    subKey( 'i', modeChange );
}

void exitSheet( void )
{
    freeMap( s.cells );
}

struct cell * newC( struct pos * p )
{
    struct cell * c = malloc( sizeof(struct cell) );
    c->txt = NULL;
    c->uFlag = false;
    c->p = p;

    mapAdd( s.cells, p, c );

    return c;
}

struct cell * findCellP( struct map * cells,
                         struct pos p )
{
    return (struct cell *) mapFind( cells, &p );
}
struct cell * findCellP2( struct map * cells,
                          unsigned int row,
                          unsigned int col )
{
    struct pos p;
    p.row = row;
    p.col = col;

    return (struct cell *) mapFind( cells, &p );
}

struct cell * getCellP( struct list * l,
                        unsigned int i )
{
    return (struct cell *) get( l, i );
}

struct pos getPos( struct list * l,
                   unsigned int i )
{
    return *(struct pos *) get( l, i );
}
