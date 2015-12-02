#ifndef CURSES_CTRL_H
#define CURSES_CTRL_H

#include "sheet.h"


void initCurses( void );
void exitCurses( void );

void drawCell( const struct sheet * const s, const struct cell * const c );

void drawHeaders( struct sheet * s );


#endif
