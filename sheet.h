#ifndef SHEET_H
#define SHEET_H

#include "maindefs.h"

#include "listlib.h"
#include <stdbool.h>

enum mode
{
    MODE_NAVIG,
    MODE_EDIT
};

struct sheet
{
    struct map * cells;

    enum mode mode;

    int rowOff;
    int colOff;

    uint wH;
    uint wW;

    uint hW;
    uint hH;

    uint oldHW;

    uint cW;
    uint cH;

    uint fH;
    uint fW;

    int curRow;
    int curCol;
    int prevRow;
    int prevCol;

    uint lastR;
    uint lastC;

    bool draw;
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
    char * res;

    struct pos * p;

    bool uFlag;
};

struct pos getPos( struct list * l,
                   uint i );

struct cell * findCellP( struct map * cells,
                         struct pos p );
struct cell * findCellP2( struct map * cells,
                          uint row,
                          uint col );
struct cell * getCellP( struct list * l,
                        uint i );

struct cell * newC( struct pos * p );

void update_cell( struct cell * c );

void initSheet( void );
void exitSheet( void );

#endif
