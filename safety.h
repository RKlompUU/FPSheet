#ifndef SAFETY_H
#define SAFETY_H

#include "crash.h"

#ifdef DEBUG
#define ALLOC_CHECKS
#define PARAM_CHECKS

#define DEBUG_ASSERTS
#endif

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

#ifdef DEBUG_ASSERTS
#define DEBUG_ASSERT(b) \
    if( !(b) ) \
    { \
      char str___[300]; \
      sprintf(str___, "Assertion failed! %s:%d", __FILE__, __LINE__); \
      crash( str___ ); \
    }
#else
#define DEBUG_ASSERT( b ) ;
#endif

#endif
