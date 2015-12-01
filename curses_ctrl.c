#include "curses_ctrl.h"

#include <ncurses.h>

void initCurses( void )
{
  initscr();
  printw("Hello World!");
  refresh();
  getch();
  endwin();
}
