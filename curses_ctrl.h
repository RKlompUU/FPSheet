#ifndef CURSES_CTRL_H
#define CURSES_CTRL_H

#include "sheet.h"

#include <ncurses.h>

void initCurses( void );
void exitCurses( bool cleanupMem );
bool isCursesEnabled( void );
void cursesCtrlLoop( void );

void drawSheet( void );
void drawCell( const struct cell * const c );
void drawHeaders( void );
void drawCursor( void );
void drawFooter( void );

enum alignment
{
  ALIGN_LEFT,
  ALIGN_CENTER,
  ALIGN_RIGHT
};

void cellWindowPos( uint r, uint c, uint * x, uint * y, enum alignment alignHor, enum alignment alignVert );
void cellWindowPos_( uint r, uint c, uint hH, uint hW, uint * x, uint * y, enum alignment alignHor, enum alignment alignVert );

void subKey( int k, void (*callback)(int) );
void handleEvent( int k );

void mvaddchu( const uint x, const uint y, const uint c );


#endif
