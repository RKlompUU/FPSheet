#ifndef SAFETY_H
#define SAFETY_H

#include "crash.h"


#define ALLOC_CHECKS
#define PARAM_CHECKS

#ifdef ALLOC_CHECKS
#include <stdlib.h> // perror
#include <stdio.h> // exit

#define CHECK_ALLOC( p ) \
  if( p == NULL ) \
  { \
    crash( "Allocation failed!" ); \
  }
#else
#define CHECK_ALLOC( p ) ;
#endif


#endif
