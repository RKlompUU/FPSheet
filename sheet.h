#ifndef SHEET_H
#define SHEET_H

#include "maindefs.h"

#include "listlib.h"
#include <stdbool.h>

enum mode
{
    MODE_NAVIG,
    MODE_EDIT,
    MODE_VISUAL,
    MODE_COMMAND
};

enum cursor_mode
{
  CMODE_MINIMAL,
  CMODE_NATURAL
};

struct pos
{
    uint row;
    uint col;
};


struct sheet
{
    struct map * cells;

    char * fileName;
    char * cmd;

    enum mode mode;
    enum cursor_mode cmode;

    int rowOff;
    int colOff;

    uint wH;
    uint wW;

    uint hW;
    uint hH;

    uint oldHW;

    uint cW;
    uint cH;
    uint bW;
    uint bH;

    uint fH;
    uint fW;

    int curRow;
    int curCol;
    int prevRow;
    int prevCol;

    uint lastR;
    uint lastC;

    uint editCursor;
    bool delete;

    bool draw;
};

extern struct sheet s;

struct cell
{
    char * txt;
    char * res;

    struct pos * p;

    bool uFlag;
    bool bar;

    struct list deps;
};

void moveCursor( int r, int c );

void addDep(
    struct cell * pDepCell,
    uint row,
    uint col );
void resetUFlags( struct cell * pcell );

struct pos getPos( struct list * l,
                   uint i );

struct cell * findCellP( struct map * cells,
                         struct pos p );
struct cell * findCellP2( struct map * cells,
                          uint row,
                          uint col );
struct cell * findCellP2_(
    struct map * cells,
    unsigned int row,
    unsigned int col );
struct cell * getCellP( struct list * l,
                        uint i );

struct cell * newC( struct pos * p );
void deleteC( void * c );

void updateCell( struct cell * c );

void initSheet( void );
void exitSheet( void );

void saveSheet( void );
void openSheet( const char * fileName );

#endif
