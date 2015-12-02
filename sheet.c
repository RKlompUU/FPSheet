#include "sheet.h"
#include "strlib.h"

#include <stdlib.h>




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
