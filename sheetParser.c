#include "sheetParser.h"

#include <stdio.h>

#include "debug.h"
#include "mpc.h"
#include "strlib.h"

typedef mpc_ast_t ast;
#define get_child mpc_ast_get_child
#define get_child_lb mpc_ast_get_child_lb

luint grab_luint( ast * a )
{
    char * pEnd;
    return strtoul( a->contents, &pEnd, 10 );
}

char * grab_str( ast * a )
{
    return copyStr( a->contents );
}


void process_cell( ast * cAst )
{
    struct pos * p = malloc( sizeof(struct pos) );
    p->row = (uint)grab_luint( get_child_lb(cAst, "num|regex", 0) );
    p->col = (uint)grab_luint( get_child_lb(cAst, "num|regex", 1) );
    struct cell * c = newC( p );
    c->txt = grab_str( get_child(cAst, "str|regex") );
}

void parseSheet( const char * fileName )
{
    mpc_parser_t * sheet = mpc_new( "sheet" );
    mpc_parser_t * meta  = mpc_new( "meta" );
    mpc_parser_t * cell  = mpc_new( "cell" );
    mpc_parser_t * num   = mpc_new( "num" );
    mpc_parser_t * str   = mpc_new( "str" );

    // big example: https://github.com/howerj/dbcc/blob/master/parse.c
    mpca_lang( MPCA_LANG_DEFAULT,
               " num   : /[0-9]+/; "
               " meta  : <num> <num>; "
               " str   : /[^\n]*/; "
               " cell  : <num> <num> <str>; "
               " sheet : /^/ <meta> <cell>* /$/; ",
               num, meta, str, cell, sheet, NULL );

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

        ast * metaAst = get_child( rootAst, "meta|>" );
        s.curRow = (int)grab_luint( get_child_lb(metaAst, "num|regex", 0) );
        s.curCol = (int)grab_luint( get_child_lb(metaAst, "num|regex", 1) );

        int cells = rootAst->children_num-3; // -3: /^/, meta and /$/
        for( int i = 0; i < cells; i++ )
        {
            process_cell( get_child_lb( rootAst, "cell|>", i+2) );
        }

        mpc_ast_delete( r.output );
        dump_txt( "****************\n" );
    }
    else
    {
        FILE * fDump = dumpFile();
        dump_txt( "parse error****\n" );
        mpc_err_print_to( r.error, fDump );
        mpc_err_delete( r.error );
        dump_txt( "***************\n" );
    }

    mpc_cleanup( 4, num, sheet, str, meta, cell );
}
