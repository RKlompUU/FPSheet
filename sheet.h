#ifndef SHEET_H
#define SHEET_H

#include "listlib.h"
#include <stdbool.h>

struct sheet
{
  struct map * cells;

  unsigned int rowOff;
  unsigned int colOff;

  unsigned int scrHeight;
  unsigned int scrWidth;
};

struct pos
{
  unsigned int row;
  unsigned int col;
};

struct cell
{
  char * txt;

  struct pos * p;

  bool uFlag;
};

struct pos getPos( struct list * l, unsigned int i );

struct cell * findCellP( struct map * cells, struct pos p );
struct cell * findCellP2( struct map * cells, unsigned int row, unsigned int col );
struct cell * getCellP( struct list * l, unsigned int i );

struct cell * newC( const char * txt, struct pos * p );


#endif
