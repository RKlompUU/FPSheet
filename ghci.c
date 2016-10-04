#include "maindefs.h"
#include "ghci.h"

#include <string.h>
#include "crash.h"
#include <unistd.h> // fork
#include <stdio.h>
#include <signal.h>
#include <sys/prctl.h>
#include <stdlib.h>
#include <fcntl.h>

#include "debug.h"

// pipe1: parent[1] -> child[0]
static int ghci_pipe1[2];
// pipe2: parent[0] <- child[1]
static int ghci_pipe2[2];

static void readall( FILE* in )
{
    char c;
    while( (c = fgetc(in)) != '>' )
    {
        putchar( c );
    }
    putchar( c );
    c = fgetc( in );
    printf( "%c\n", c );
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

        sleep( 1 );
        FILE* in = fdopen( ghci_pipe2[0], "r" );

        readall(in);
        write_ghci( "let x = 5\n" );
        readall(in);
        write_ghci( "x\n" );
        readall(in);
        write_ghci( "x\n" );
        readall(in);
        write_ghci( "y\n" );
        readall(in);

        exit(1);

        printf( read_ghci() );
        printf( "\n" );
        sleep( 1 );
        printf( read_ghci() );
        printf( "\n" );
        sleep( 1 );
        write_ghci( "x\n" );
        printf( read_ghci() );
        printf( "\n" );
        sleep( 1 );
        printf( read_ghci() );
        printf( "\n" );

        exit(0);
        return;
    }
}

void write_ghci( const char * str )
{
    //FILE* ghci_in = fdopen( ghci_pipe1[1], "w" );
    write( ghci_pipe1[1], str, strlen(str) );
    //fclose( ghci_in );
}

const char * read_ghci( void )
{
    //FILE* ghci_out = fdopen( ghci_pipe2[0], "r" );
    char * str = malloc( sizeof(char) * 1000 );
    read( ghci_pipe2[0], str, 1000 );
    //fclose( ghci_out );
    return str;
}
