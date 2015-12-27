#ifndef SHEET_H
#define SHEET_H

#include "maindefs.h"

#include "listlib.h"
#include <stdbool.h>

struct sheet
{
  struct map * cells;

  int rowOff;
  int colOff;

  uint wH;
  uint wW;

  uint hW;
  uint hH;

  uint cW;
  uint cH;
};

extern struct sheet s;

struct pos
{
  uint row;
  uint col;
};

struct cell
{
  char * txt;

  struct pos * p;

  bool uFlag;
};

struct pos getPos( struct list * l, uint i );

struct cell * findCellP( struct map * cells, struct pos p );
struct cell * findCellP2( struct map * cells, uint row, uint col );
struct cell * getCellP( struct list * l, uint i );

struct cell * newC( const char * txt, struct pos * p );

void initSheet( void );
void exitSheet( void );


#endif
