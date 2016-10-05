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
static int ghci_pipe1[2];
// pipe2: parent[0] <- child[1]
static int ghci_pipe2[2];

FILE* ghci_out;

static void readall( FILE* in )
{
    int c;
    while( (c = fgetc(in)) != '>' )
    {
        //putchar( c );
    }
    //putchar( c );
    c = fgetc( in );
    //printf( "%c\n", c );
}

void init_ghci( void )
{
    if( pipe(ghci_pipe1) )
        crash( "failed to create pipe for ghci" );
    if( pipe(ghci_pipe2) )
        crash( "failed to create pipe for ghci" );

    int pid = fork();
    if( pid == 0 )
    {
        close( ghci_pipe1[1] );
        close( ghci_pipe2[0] );

        dup2( ghci_pipe1[0], STDIN_FILENO );
        dup2( ghci_pipe2[1], STDOUT_FILENO );
        //int fnull = open( "/dev/null", O_WRONLY );
        FILE* ferr = fopen( "errs_ghci.txt", "w" );
        //dup2( fnull, STDERR_FILENO );
        dup2( fileno(ferr), STDERR_FILENO );
        //close( STDERR_FILENO );

        prctl( PR_SET_PDEATHSIG, SIGHUP );

        // Child process
        execl( "/usr/bin/ghci", "/usr/bin/ghci", NULL );
    }
    else
    {
        // Parent process
        close( ghci_pipe1[0] );
        close( ghci_pipe2[1] );

        ghci_out = fdopen( ghci_pipe2[0], "r" );

        readall( ghci_out );
        return;
    }
}

static void read_nonblock( void )
{
    fcntl( ghci_pipe2[0], F_SETFL, O_NONBLOCK );
}
static void read_block( void )
{
    int oldfl = fcntl( ghci_pipe2[0], F_GETFL );
    fcntl( ghci_pipe2[0], F_SETFL, oldfl & ~O_NONBLOCK );
}

char * ghci_exec( const char * expr )
{
    write( ghci_pipe1[1], expr, strlen(expr) );

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
        for( int i = 0; i < 3; i++ )
        {
            usleep( 100 );
            c = fgetc( ghci_out );
            if( c != -1 )
            {
                if( c == '\n' )
                    nls++;
                appendChar( &res, (char)c );
                finished = false;
                break;
            }
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
