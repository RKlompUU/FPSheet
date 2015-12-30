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
    char str___[300]; \
    sprintf(str___, "Allocation failed! %s:%d", __FILE__, __LINE__); \
    crash( str___ ); \
  }
#else
#define CHECK_ALLOC( p ) ;
#endif


#endif
