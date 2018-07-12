#include "maindefs.h"
#include "ghci.h"

#include <stdbool.h>

#include <string.h>
#include "crash.h"
#include <unistd.h> // fork
#include <stdio.h>
#include <signal.h>
#include <sys/prctl.h>
#include <stdlib.h>
#include <fcntl.h>

#include "strlib.h"
#include "debug.h"

// pipe1: parent[1] -> child[0]
static int ghci_inpipe[2];
// pipe2: parent[0] <- child[1]
static int ghci_outpipe[2];

FILE* ghci_out;

static void readall( FILE* in )
{
    int c;
    while( (c = fgetc(in)) != '>' )
    {
    }
    c = fgetc( in );
}

static int try_read( FILE* in, int tries, uint timeoutLength )
{
    for( int i = 0; i < tries; i++ )
    {
        usleep( timeoutLength );
        int c = fgetc( ghci_out );
        if( c != -1 )
            return c;
    }
    return -1;
}

void init_ghci( void )
{
    if( pipe(ghci_inpipe) )
        crash( "failed to create pipe for ghci" );
    if( pipe(ghci_outpipe) )
        crash( "failed to create pipe for ghci" );

    int pid = fork();
    if( pid == 0 )
    {
        close( ghci_inpipe[1] );
        close( ghci_outpipe[0] );

        dup2( ghci_inpipe[0], STDIN_FILENO );
        dup2( ghci_outpipe[1], STDOUT_FILENO );
        FILE* ferr = fopen( "errs_ghci.txt", "w" );
        dup2( fileno(ferr), STDERR_FILENO );

        prctl( PR_SET_PDEATHSIG, SIGHUP );

        // Child process
        execl( "/usr/bin/ghci", "/usr/bin/ghci", NULL );
    }
    else
    {
        // Parent process
        close( ghci_inpipe[0] );
        close( ghci_outpipe[1] );

        ghci_out = fdopen( ghci_outpipe[0], "r" );

        printf( "Starting ghci backend...\n" );
        readall( ghci_out );
        return;
    }
}

bool ghci_check_err( void )
{
    bool res = false;
    for( int attempts = 0; attempts < 3; attempts++ )
    {
        FILE* f = fopen( "errs_ghci.txt", "r" );
        fseek( f, 0L, SEEK_END );
        if( ftell(f) )
        {
            res = true;
            attempts = 0;
            fclose( f );
            f = fopen( "errs_ghci.txt", "w" );
            fclose( f );
        }

        usleep( 100 );
    }

    return res;
}

static void read_nonblock( void )
{
    fcntl( ghci_outpipe[0], F_SETFL, O_NONBLOCK );
}
static void read_block( void )
{
    int oldfl = fcntl( ghci_outpipe[0], F_GETFL );
    fcntl( ghci_outpipe[0], F_SETFL, oldfl & ~O_NONBLOCK );
}

char * ghci_exec( const char * expr )
{
    write( ghci_inpipe[1], expr, strlen(expr) );

    char * res = malloc( sizeof(char) );
    res[0] = '\0';

    int c;
    int nls = 0;
    bool finished = false;
    do
    {
        read_block();
        while( (c = fgetc(ghci_out)) != '>' )
        {
            if( c == '\n' )
                nls++;
            appendChar( &res, (char)c );
        }
        appendChar( &res, (char) c );
        c = fgetc( ghci_out );
        if( c == '\n' )
            nls++;
        appendChar( &res, (char)c );

        read_nonblock();
        finished = true;
        c = try_read( ghci_out, 3, 100 );
        if( c != -1 )
        {
            if( c == '\n' )
                nls++;
            appendChar( &res, (char)c );
            finished = false;
        }
    }
    while( !finished );

    if( nls == 0 )
    {
        free( res );
        return NULL;
    }

    char * nl = strrchr( res, '\n' );
    res[nl-res] = '\0';

    return res;
}
