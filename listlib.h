#ifndef LISTLIB_H
#define LISTLIB_H

struct list
{
  void ** xs;
  unsigned int size;
  unsigned int allocated;
};

struct list * allocList( void );
void freeList( struct list * l );

void push_back( struct list * l, void * x );
void * get( struct list * l, unsigned int i );

int getI( struct list * l, unsigned int i );


#endif
