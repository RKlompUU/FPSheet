#include "sheet.h"
#include "strlib.h"

#include <stdlib.h>

#include <string.h>
#include <curses.h>

#include "curses_ctrl.h"
#include "ghci.h"
#include <ctype.h>

#include "debug.h"

#include "parsers.h"

struct sheet s;

//
// Utility functions
//

static int posCmp( void * p1_, void * p2_ )
{
  struct pos * p1 = (struct pos *) p1_;
  struct pos * p2 = (struct pos *) p2_;

  if ( p1->row < p2->row ) return -1;
  if ( p1->row > p2->row ) return 1;

  if ( p1->col < p2->col ) return -1;
  if ( p1->col > p2->col ) return 1;

  return 0;
}

static bool cmpCells( void * c1_, void * c2_ )
{
  struct cell * c1 = (struct cell *)c1_;
  struct cell * c2 = (struct cell *)c2_;

  return (c1->p->row == c2->p->row) &&
         (c1->p->col == c2->p->col);
}

static void gotoOff( int r, int c )
{
  if ( r < 0 || c < 0 ) return;

  s.rowOff = r;
  s.colOff = c;

  s.draw = true;
}

void moveCursor( int r, int c )
{
  if ( r < 0 || c < 0 ) return;

  s.curRow = r;
  s.curCol = c;

  s.draw = true;

  if( r < s.rowOff ) gotoOff( r, s.colOff );
  if( c < s.colOff ) gotoOff( s.rowOff, c );
  int rOver = r - (int) s.lastR;
  int cOver = c - (int) s.lastC;
  if( rOver > 0 && cOver > 0 ) gotoOff( s.rowOff + rOver, s.colOff + cOver );
  else if( rOver > 0 ) gotoOff( s.rowOff + rOver, s.colOff );
  else if( cOver > 0 ) gotoOff( s.rowOff, s.colOff + cOver );
}

static void editStrCallback( int k )
{
  switch( k )
  {
    case KEY_BACKSPACE:
    case 127:
      {
        long unsigned int length = strlen( *s.editStr );
        if( s.editCursor > 0 && length > 0 )
        {
          s.editCursor--;
          removeChar( s.editStr, s.editCursor );
          s.draw = true;
        }
      }
      break;
    case KEY_LEFT:
      if( s.editCursor > 0 )
      {
        s.editCursor--;
        s.draw = true;
      }
      break;
    case KEY_RIGHT:
      if( s.editCursor < strlen(*s.editStr) )
      {
        s.editCursor++;
        s.draw = true;
      }
      break;
    default:
      insertChar( s.editStr, (char) k, s.editCursor );
      s.editCursor++;
      s.draw = true;
      break;
  }
}

void enterStrEdit( char ** str )
{
  s.editStr = str;
  s.editCursor = strlen( *str );

  subGroup( GROUP_SUB_EDIT, editStrCallback );
}

void exitStrEdit( void )
{
  unsubGroup( GROUP_SUB_EDIT, editStrCallback );
}

static void moveCursorKey( int k )
{
  switch ( k )
  {
    case KEY_UP:
      moveCursor( s.curRow - 1, s.curCol );
      break;
    case KEY_DOWN:
      moveCursor( s.curRow + 1, s.curCol );
      break;
    case KEY_LEFT:
      moveCursor( s.curRow, s.curCol - 1 );
      break;
    case KEY_RIGHT:
      moveCursor( s.curRow, s.curCol + 1 );
      break;
  }
}

static void toggleBar( uint row, uint col )
{
  struct pos * p = malloc( sizeof(struct pos) );
  p->row = row;
  p->col = col;
  struct cell * c = mapFind( s.cells, p );
  if( c )
  {
    c->bar = !c->bar;
  }
  else
  {
    c = newC( p );
    c->bar = true;
    c->txt = malloc( sizeof(char)*1 );
    c->txt[0] = '\0';
  }
}

static void visualCallback( int k )
{
  switch( k )
  {
    case KEY_UP:
    case KEY_DOWN:
    case KEY_LEFT:
    case KEY_RIGHT:
      s.delete = false;
      moveCursorKey( k );
      break;
    case 'u':
      {
        s.delete = false;
        toggleBar( s.curRow, s.curCol );
        s.draw = true;
      }
      break;
    case 'd':
      if( s.delete )
      {
        s.delete = false;
        struct cell * c = findCellP2( s.cells, (uint) s.curRow, (uint) s.curCol );
        if( c )
        {
          if( c->bar )
            c->bar = false;
          if( c->txt )
            c->txt[0] = '\0';
          if( c->res )
            c->res[0] = '\0';
          s.draw = true;
        }
      }
      else
      {
        s.delete = true;
      }
      break;
  }
}

void modeChange( int k )
{
  switch ( s.mode )
  {
    case MODE_NAVIG:
      switch ( k )
      {
        case KEY_ENTER:
        case '\r':
        case 'i':
          unsubGroup( GROUP_SUB_NAVIG, moveCursorKey );

          struct cell * c = findCellP2_( s.cells, (uint) s.curRow, (uint) s.curCol );
          enterStrEdit( &c->txt );

          s.mode = MODE_EDIT;
          s.draw = true;

          if ( c->res )
          {
            free( c->res );
            c->res = NULL;
          }

          break;
        case 'v':
          unsubGroup( GROUP_SUB_NAVIG, moveCursorKey );
          subGroup( GROUP_SUB_VISUAL, visualCallback );

          s.mode = MODE_VISUAL;
          s.draw = true;
          break;
        case ':':
          unsubGroup( GROUP_SUB_NAVIG, moveCursorKey );
          enterStrEdit( &s.cmd );

          s.mode = MODE_COMMAND;
          s.draw = true;
          break;
      }
      break;
    case MODE_EDIT:
      switch ( k )
      {
        case KEY_ENTER:
        case '\r':
          exitStrEdit();
          subGroup( GROUP_SUB_NAVIG, moveCursorKey );

          s.mode = MODE_NAVIG;
          s.draw = true;
          struct cell * c = findCellP2( s.cells, (uint) s.curRow, (uint) s.curCol );
          if ( c )
          {
            dump_txt( "------\nupdating root cell\n" );
            updateCell( c );
            resetUFlags( c );
          }
          break;
        case KEY_ESC:
          exitStrEdit();
          subGroup( GROUP_SUB_NAVIG, moveCursorKey );

          undo();

          s.mode = MODE_NAVIG;
          s.draw = true;
          break;
      }
      break;
    case MODE_VISUAL:
      switch( k )
      {
        case KEY_ESC:
          unsubGroup( GROUP_SUB_VISUAL, visualCallback );
          subGroup( GROUP_SUB_NAVIG, moveCursorKey );

          s.delete = false;
          s.mode = MODE_NAVIG;
          s.draw = true;
      }
      break;
    case MODE_COMMAND:
      switch( k )
      {
        case KEY_ENTER:
        case '\r':
          exitStrEdit();
          subGroup( GROUP_SUB_NAVIG, moveCursorKey );

          parseCommand( s.cmd );

          s.cmd[0] = '\0';
          s.mode = MODE_NAVIG;
          s.draw = true;
          break;
        case KEY_ESC:
          exitStrEdit();
          subGroup( GROUP_SUB_NAVIG, moveCursorKey );

          s.cmd[0] = '\0';
          s.mode = MODE_NAVIG;
          s.draw = true;
          break;

      }
      break;
  }
}

void addDep( struct cell * pDepCell, uint row, uint col )
{
  struct cell * c = findCellP2_( s.cells, row, col );
  int i = findElem( &c->deps, pDepCell, cmpCells );
  if( i < 0 )
  {
#ifdef DEBUG
    dump_txt( "New dependency registered\n" );
#endif
    pushBack( &c->deps, pDepCell );
  }
}

void resetUFlags( struct cell * pcell )
{
  if( pcell->uFlag == false )
    return;
  pcell->uFlag = false;

  for( uint i = 0; i < pcell->deps.size; i++ )
    resetUFlags( (struct cell *)get(&pcell->deps, i) );
}

void updateCell( struct cell * pcell )
{
  dump_txt( "entering updateCell\n" );
  if( pcell->uFlag )
    return;
  pcell->uFlag = true;

  char * letDef = copyStr( pcell->txt );
  for( uint i = 0; i < strlen(letDef); i++ )
  {
    char c = letDef[i];
    if( c >= 'A' && c <= 'Z' )
    {
      if( iswordPosRef(&letDef[i]) )
      {
        uint refStrLength = wordLength( &letDef[i] );

        uint refR, refC;
        refStr2Pos( &letDef[i], refStrLength, &refR, &refC );

#ifdef DEBUG
        char * rS = uiStr( refR );
        char * cS = uiStr( refC );

        dump_txt( "ref in cel: " );
        dump_txt( rS );
        dump_txt( "," );
        dump_txt( cS );
        dump_txt( "\n" );

        free( rS );
        free( cS );
#endif
        addDep( pcell, refR, refC );

        letDef[i] = (char) tolower( letDef[i] );
      }
    }

    for( ; i < strlen(letDef); i++ )
    {
      if( letDef[i] == ' ' )
        break;
    }
  }
  if( letDef[0] == ':' )
  {
    appendChar( &letDef, '\n' );
    dump_txt( letDef );
    if( pcell->res )
      free( pcell->res );
    pcell->res = ghci_exec( letDef );
    if( pcell->res )
      dump_txt( pcell->res );
    ghci_check_err();
    free( letDef );
    goto UPDATE_CHILDS_RETURN;
  }

  char * letName = curPos2Str( pcell->p->row, pcell->p->col );
  letName[0] = (char)tolower( letName[0] );

  //"let " letName " = " letDef "\n"
  //4               3            2
  size_t length = 4 + strlen(letName) + 3 + strlen(letDef) + 2;
  char * letCommand = malloc( sizeof(char) * length );
  sprintf( letCommand, "let %s = %s\n", letName, letDef );

  free( letDef );
  appendChar( &letName, '\n' );

  dump_txt( letName );
  dump_txt( letCommand );
  ghci_exec( letCommand );
  if( !ghci_check_err() )
  {
    if( pcell->res )
      free( pcell->res );
    pcell->res = ghci_exec( letName );
    if( !pcell->res )
      ghci_check_err(); // Consume the error
  }
  if( pcell->res )
  {
    dump_txt( pcell->res );
    dump_txt( "\n" );
  }
  else
    dump_txt( "no result\n" );
  free( letName );
  free( letCommand );

UPDATE_CHILDS_RETURN:
  for( uint i = 0; i < pcell->deps.size; i++ )
    updateCell( (struct cell *)get(&pcell->deps, i) );
}

void saveSheet( void )
{
  if( !s.fileName )
  {
    dump_txt( "save to file request, but filename is NULL\n" );
    return; // TODO: provide feedback to user
  }

  FILE * f = fopen( s.fileName, "w" );
  if( !f )
  {
    dump_txt( "Failed to open savefile!" );
    return;
  }

  /*
   * File format:
   *
   * curRow curCol rowOff colOff
   * 'c'
   * cells
   *
   * cell: row col bar :txt
   * bar: 0 | 1
   */

  fprintf( f, "%d %d %d %d\nc\n", s.curRow, s.curCol, s.rowOff, s.colOff );
  for( uint i = 0; i < *s.cells->pSize; i++ )
  {
    struct cell * c = getVal( s.cells, i );
    if( c->bar || strlen(c->txt) )
      fprintf( f, "%d %d %d :%s\n", c->p->row, c->p->col, (c->bar ? 1 : 0), c->txt );
  }

  fclose( f );
}

void openSheet( const char * fileName )
{
  parseSheet( fileName );
  s.fileName = copyStr( fileName ) ;
  drawHeaders(); // To set lastR and lastC
  moveCursor( s.curRow, s.curCol );

  for( uint i = 0; i < s.cells->vals->size; i++ )
  {
    struct cell * c = (struct cell *)get( s.cells->vals, i );
    updateCell( c );
    resetUFlags( c );
  }

  return;
}

void initSheet( void )
{
  s.cells = allocMap( posCmp );
  s.fileName = NULL;

  s.rowOff = 0;
  s.colOff = 0;

  s.cmd = malloc( sizeof(char) );
  s.cmd[0] = '\0';

  int h, w;
  getmaxyx( stdscr, h, w );
  s.wH = (uint) h;
  s.wW = (uint) w;

  s.hW = 2;
  s.hH = 2;

  s.cW = 10;
  s.cH = 1;
  s.bW = 1;
  s.bH = 0;

  s.fH = 1;
  s.fW = s.wW;

  s.curRow = 0;
  s.curCol = 0;

  s.prevRow = 0;
  s.prevCol = 0;

  s.editCursor = 0;
  s.delete = false;

  s.draw = true;

  s.mode = MODE_NAVIG;
  s.cmode = CMODE_NATURAL;

  addSubToGroup( KEY_UP, GROUP_SUB_NAVIG );
  addSubToGroup( KEY_DOWN, GROUP_SUB_NAVIG );
  addSubToGroup( KEY_LEFT, GROUP_SUB_NAVIG );
  addSubToGroup( KEY_RIGHT, GROUP_SUB_NAVIG );

  for ( int c = ' '; c <= '~'; c++ )
  {
    addSubToGroup( c, GROUP_SUB_EDIT );
    addSubToGroup( c, GROUP_SUB_CMD );
  }
  addSubToGroup( KEY_BACKSPACE, GROUP_SUB_EDIT );
  addSubToGroup( 127, GROUP_SUB_EDIT );

  addSubToGroup( KEY_LEFT, GROUP_SUB_EDIT );
  addSubToGroup( KEY_RIGHT, GROUP_SUB_EDIT );

  addSubToGroup( KEY_BACKSPACE, GROUP_SUB_CMD );
  addSubToGroup( 127, GROUP_SUB_CMD );

  subGroup( GROUP_SUB_NAVIG, moveCursorKey );

  subKey( KEY_ESC, modeChange );
  subKey( KEY_ENTER, modeChange );
  subKey( '\r', modeChange );
  subKey( 'i', modeChange );
  subKey( 'v', modeChange );
  subKey( ':', modeChange );

  addSubToGroup( KEY_UP, GROUP_SUB_VISUAL );
  addSubToGroup( KEY_DOWN, GROUP_SUB_VISUAL );
  addSubToGroup( KEY_LEFT, GROUP_SUB_VISUAL );
  addSubToGroup( KEY_RIGHT, GROUP_SUB_VISUAL );
  addSubToGroup( KEY_ENTER, GROUP_SUB_VISUAL );
  addSubToGroup( '\r', GROUP_SUB_VISUAL );
  addSubToGroup( 'u', GROUP_SUB_VISUAL );
  addSubToGroup( 'd', GROUP_SUB_VISUAL );
  addSubToGroup( KEY_ESC, GROUP_SUB_VISUAL );

  openSheet( ".sheet" );
}

void exitSheet( void )
{
  saveSheet();
  freeMap( s.cells, free, deleteC );

  free( s.fileName );
  s.fileName = NULL;

  free( s.cmd );
  s.cmd = NULL;
}

struct cell * newC( struct pos * p )
{
  struct cell * c = malloc( sizeof(struct cell) );
  c->txt = NULL;
  c->uFlag = false;
  c->p = p;
  c->res = NULL;
  c->bar = false;

  initList( &c->deps );

  mapAdd( s.cells, p, c );

  return c;
}

void deleteC( void * c_ )
{
  struct cell * c = (struct cell *)c_;
  freeListExcl( &c->deps, identity );
  if( c->txt )
    free( c->txt );
  if( c->res )
    free( c->res );
  free( c );
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

struct cell * findCellP2_( struct map * cells, unsigned int row, unsigned int col )
{
  struct cell * c = findCellP2( cells, row, col );
  if ( c == NULL )
  {
    struct pos * p = malloc( sizeof(struct pos) );
    p->col = col;
    p->row = row;
    c = newC( p );
    c->txt = malloc( sizeof(char) );
    c->txt[0] = '\0';
  }
  return c;
}


struct cell * getCellP( struct list * l, unsigned int i )
{
  return (struct cell *) get( l, i );
}

struct pos getPos( struct list * l, unsigned int i )
{
  return *(struct pos *) get( l, i );
}

void undo( void )
{
  dump_txt( "undo\n" );
}

void redo( void )
{
  dump_txt( "redo\n" );
}
