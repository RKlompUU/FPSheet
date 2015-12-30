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
void drawFooter( void );

void subKey( int k, void (*callback)(int) );
void handleEvent( int k );


#endif
