#include "parsers.h"

#include <stdio.h>

#include "debug.h"
#include "mpc.h"
#include "strlib.h"
#include "ghci.h"

#include "sheet.h"
#include "curses_ctrl.h"

typedef mpc_ast_t ast;
#define get_child mpc_ast_get_child
#define get_child_lb mpc_ast_get_child_lb

luint grabLuint( ast * a )
{
    char * pEnd;
    return strtoul( a->contents, &pEnd, 10 );
}

char * grabStr( ast * a, int off )
{
    return copyStr( &a->contents[off] );
}

void processCell( ast * cAst )
{
    struct pos * p = malloc( sizeof(struct pos) );
    p->row = (uint)grabLuint( get_child_lb(cAst, "num|regex", 0) );
    p->col = (uint)grabLuint( get_child_lb(cAst, "num|regex", 1) );
    struct cell * c = newC( p );
    c->txt = grabStr( get_child(cAst, "str|regex"), 1 );
    c->bar = get_child(cAst, "bool|char")->contents[0] == '1';
}


struct cmdListEntry
{
    const char * lbl;
    void (*processCmd)(ast *);
};

static void addCmd2List( struct list * l, void (*processCmd)(ast *), const char * lbl )
{
    struct cmdListEntry * c = malloc( sizeof(struct cmdListEntry) );
    c->lbl = lbl;
    c->processCmd = processCmd;
    pushBack( l, c );
}

static void processGotoCell( ast * gotoAst )
{
    luint r = grabLuint( get_child(gotoAst, "row|regex") );
    luint c = alpha2Uint( get_child(gotoAst, "col|regex")->contents );

    moveCursor( r, c );
}
static void processGotoRow( ast * gotoAst )
{
    luint r = grabLuint( get_child(gotoAst, "row|regex") );

    moveCursor( r, s.curCol );
}
static void processGotoCol( ast * gotoAst )
{
    luint c = alpha2Uint( get_child(gotoAst, "col|regex")->contents );

    moveCursor( s.curRow, c );
}
static void processQuit( ast * quitAst )
{
    stopCurses();
}

void parseCommand( const char * str )
{
    mpc_parser_t * row      = mpc_new( "row" );
    mpc_parser_t * col      = mpc_new( "col" );
    mpc_parser_t * gotoCell = mpc_new( "gotoCell" );
    mpc_parser_t * gotoRow  = mpc_new( "gotoRow" );
    mpc_parser_t * gotoCol  = mpc_new( "gotoCol" );
    mpc_parser_t * quit     = mpc_new( "quit" );
    mpc_parser_t * command  = mpc_new( "command" );

    mpca_lang( MPCA_LANG_WHITESPACE_SENSITIVE,
               " row      : /[0-9]+/; "
               " col      : /[A-Z]+/; "
               " gotoCell : ':' <row> <col>; "
               " gotoCol  : ':' <col>; "
               " gotoRow  : ':' <row>; "
               " quit     : ':' 'q'; "
               " command  : /^/ (<gotoCell> | <gotoRow> | <gotoCol> | <quit>) /$/; ",
               row, col, gotoCell, gotoRow, gotoCol, quit, command, NULL );

    mpc_result_t r;
    if( mpc_parse("", str, command, &r) )
    {
        FILE * fDump = dumpFile();
        dump_txt( "parse result****\n" );
        ast * rootAst = (ast*)r.output;
        mpc_ast_print_to( rootAst, fDump );
        dump_txt( "****************\n" );

        struct list l;
        initList( &l );
        addCmd2List( &l, &processGotoCell, "gotoCell|>" );
        addCmd2List( &l, &processGotoRow, "gotoRow|>" );
        addCmd2List( &l, &processGotoCol, "gotoCol|>" );
        addCmd2List( &l, &processQuit, "quit|>" );

        for( uint i = 0; i < l.size; i++ )
        {
            struct cmdListEntry * cmdEntry = get( &l, i );
            ast * a = get_child( rootAst, cmdEntry->lbl );
            if( a )
            {
                (*cmdEntry->processCmd)( a );
                break;
            }
        }

        freeListExcl( &l, free );
        mpc_ast_delete( r.output );
    }
    else
    {
        FILE * fDump = dumpFile();
        dump_txt( "parse error****\n" );
        mpc_err_print_to( r.error, fDump );
        mpc_err_delete( r.error );
        dump_txt( "***************\n" );
    }


    mpc_cleanup( 7, row, col, gotoCell, gotoRow, gotoCol, quit, command );
}

void parseSheet( const char * fileName )
{
    mpc_parser_t * sheet = mpc_new( "sheet" );
    mpc_parser_t * boolean = mpc_new( "bool" );
    mpc_parser_t * meta  = mpc_new( "meta" );
    mpc_parser_t * cell  = mpc_new( "cell" );
    mpc_parser_t * cells = mpc_new( "cells" );
    mpc_parser_t * num   = mpc_new( "num" );
    mpc_parser_t * str   = mpc_new( "str" );

    // big example: https://github.com/howerj/dbcc/blob/master/parse.c
    mpca_lang( MPCA_LANG_DEFAULT,
               " bool  : '0' | '1'; "
               " num   : /[0-9]+/; "
               " meta  : <num> <num> <num> <num>; "
               " str   : /:[^\n]*/; "
               " cell  : <num> <num> <bool> <str>; "
               " cells : 'c' <cell>*; "
               " sheet : /^/ <meta> <cells> /$/; ",
               boolean, num, meta, str, cell, cells, sheet, NULL );

    mpc_result_t r;

    FILE * f = fopen( fileName, "r" );
    if( !f )
    {
        dump_txt( "Save <" );
        dump_txt( fileName );
        dump_txt( "> doesn't exist, cold boot\n" );
        return;
    }
    if( mpc_parse_file( fileName, f, sheet, &r ) )
    {
        FILE * fDump = dumpFile();
        dump_txt( "parse result****\n" );
        ast * rootAst = (ast*)r.output;
        mpc_ast_print_to( rootAst, fDump );
        dump_txt( "****************\n" );

        ast * metaAst = get_child( rootAst, "meta|>" );
        s.curRow = (int)grabLuint( get_child_lb(metaAst, "num|regex", 0) );
        s.curCol = (int)grabLuint( get_child_lb(metaAst, "num|regex", 1) );
        s.rowOff = (int)grabLuint( get_child_lb(metaAst, "num|regex", 2) );
        s.colOff = (int)grabLuint( get_child_lb(metaAst, "num|regex", 3) );

        ast * cellsAst = get_child( rootAst, "cells|>" );
        if( cellsAst )
        {
            for( int i = 1; i < cellsAst->children_num; i++ ) // starting at 1, 'c' is at index 0
            {
                processCell( get_child_lb( cellsAst, "cell|>", i) );
            }
        }

        mpc_ast_delete( r.output );
    }
    else
    {
        FILE * fDump = dumpFile();
        dump_txt( "parse error****\n" );
        mpc_err_print_to( r.error, fDump );
        mpc_err_delete( r.error );
        dump_txt( "***************\n" );
    }

    mpc_cleanup( 7, num, sheet, str, meta, cell, cells, boolean );
    fclose( f );
}
