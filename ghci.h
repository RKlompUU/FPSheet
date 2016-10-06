#ifndef GHCI_H
#define GHCI_H

#include <stdbool.h>

void init_ghci( void );

char * ghci_exec( const char * expr );
bool ghci_check_err( void );

#endif
