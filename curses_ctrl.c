#include "curses_ctrl.h"
#include "listlib.h"
#include "safety.h"

#include "memlib.h"
#include "strlib.h"

#include <stdlib.h>

#include <ncurses.h>

#include <string.h> // strlen

// struct sheet s; < extern

struct keyListener
{
    int k;

    void (*callback)( int );
};

struct list kListeners;
struct keyListener * getListener( unsigned int i )
{
    return (struct keyListener *) get( &kListeners, i );
}

struct list subGroups; // group subscriptions

bool cursesEnabled = false;
bool isCursesEnabled( void )
{
    return cursesEnabled;
}

void initCurses( void )
{
    initscr();
    raw();
    keypad( stdscr, TRUE ); // Get function keys, arrow keys etc.
    cbreak(); // Instantly send keypress events to this application, don't wait for EOLs
    noecho();
    nonl();

    curs_set( 0 );

    initList( &kListeners );
    initList( &subGroups );
    for ( int i = GROUP_SUB_NAVIG; i <= GROUP_SUB_EDIT; i++ )
    {
        pushBack( &subGroups, allocList() );
    }

    cursesEnabled = true;
}

void exitCurses( bool cleanupMem )
{
    if ( cursesEnabled )
    {
        if ( cleanupMem )
        {
            freeListExcl( &kListeners );

            for ( uint i = 0; i < subGroups.size; i++ )
                freeList( (struct list *) get( &subGroups, i ) );
            freeListExcl( &subGroups );
        }

        endwin();
    }

    cursesEnabled = false;
}

void cursesCtrlLoop( void )
{
    while ( cursesEnabled )
    {
        if ( s.draw )
        {
            drawSheet();
            refresh();
            s.draw = false;
        }
        int k = getch();

        mvprintw( 0, 0, "Key: %u", k );
        refresh();

        handleEvent( k );
    }
}

void render( void )
{
    for ( unsigned int i = 0; i < *s.cells->pSize; i++ )
    {
        struct cell * c = getCellP( s.cells->vals, i );
        if ( c->uFlag )
        {
            drawCell( c );
            c->uFlag = false;
        }
    }
}

void drawSheet()
{
    drawHeaders();
    drawCursor();
    drawFooter();

    render();

    mvaddch( 0, 0, ' ' );
    switch( s.mode )
    {
        case MODE_NAVIG:
        mvaddch( (int)s.wH-1, 0, 'M' );
            break;
        case MODE_EDIT:
        mvaddch( (int)s.wH-1, 0, 'E' );
            break;
    }
}

void drawCell( const struct cell * const c )
{
    uint x;
    uint y;
    cellWindowPos( (uint) s.rowOff + c->p->row, (uint) s.colOff + c->p->col, &x,
                   &y, ALIGN_LEFT, ALIGN_CENTER );
    mvaddstr( (int )x, (int )y, c->txt );
}

void cleanArea( uint x1,
                uint y1,
                uint x2,
                uint y2 )
{
    for ( uint x = x1; x < x2; x++ )
    {
        for ( uint y = y1; y < y2; y++ )
        {
            mvaddch( (int )x, (int )y, ' ' );
        }
    }
}

void cellWindowPos_( uint r,
                     uint c,
                     uint hH,
                     uint hW,
                     uint * x,
                     uint * y,
                     enum alignment alignHor,
                     enum alignment alignVert )
{
    if ( x )
    {
        uint bcH = s.cH + 1;
        *x = hH + (r - (uint) s.rowOff) * bcH;

        switch ( alignVert )
        {
            case ALIGN_CENTER:
            *x += bcH / 2;
                break;
            case ALIGN_RIGHT:
            *x += bcH;
                break;
            case ALIGN_LEFT:
                break;
        }
        (*x)--;
    }

    if ( y )
    {
        uint bcW = s.cW + 1;
        *y = hW + (c - (uint) s.colOff) * bcW;

        switch ( alignHor )
        {
            case ALIGN_CENTER:
            *y += bcW / 2;
                break;
            case ALIGN_RIGHT:
            *y += bcW;
                break;
            case ALIGN_LEFT:
                break;
        }
        (*y)--;
    }
}
void cellWindowPos( uint r,
                    uint c,
                    uint * x,
                    uint * y,
                    enum alignment alignHor,
                    enum alignment alignVert )
{
    cellWindowPos_( r, c, s.hH, s.hW, x, y, alignHor, alignVert );
}

void drawHeaders( void )
{
    s.oldHW = s.hW;

    //
    // Row numbers
    //

    uint bcH = s.cH + 1; // Bordered cell height
    uint lastR = (uint) s.rowOff + (s.wH - s.hH - bcH - 1) / bcH; // - bcH, - 1 to prevent a possible half-row
    // This conversion from luint to uint is save:
    //    The length of row numbers will never even come close to the limits of uint
    //    , since long before that point other limits would've been reached already
    s.hW = (uint) uiLength( lastR ) + 1; // + 1 for the border

    if ( s.oldHW != s.hW )
        cleanArea( s.hH, 0, s.wH, s.wW );
    else
        cleanArea( s.hH, 0, s.wH, s.oldHW );

    uint x;
    for ( uint r = (uint) s.rowOff; r <= lastR; r++ )
    {
        cellWindowPos( r, 0, &x, NULL, ALIGN_CENTER, ALIGN_CENTER );
        char * str = uiStr( r );
        mvaddstr( (int )x, 0, str ); // Save conversion
        free( str );
    }
    // Row numbers border
    for ( x = s.hH; x < s.wH; x++ )
    {
        mvaddch( (int )x, (int )s.hW - 1, '|' );
    }

    s.lastR = lastR;

    //
    // Column letters
    //

    uint bcW = s.cW + 1;
    uint lastC = (uint) s.colOff + (s.wW - s.hW - bcW - 1) / bcW;

    if ( s.curCol > (int) lastC ) // Both safe convertions
        s.curCol = (int) lastC;

    cleanArea( 0, 0, s.hH, s.wW );

    uint y;
    for ( uint c = (uint) s.colOff; c <= lastC; c++ )
    {
        cellWindowPos( 0, c, NULL, &y, ALIGN_CENTER, ALIGN_CENTER );
        char * str = uint2Alpha( c );
        mvaddstr( 0, (int )y, str ); // Save conversion
        free( str );
    }

    for ( y = s.hW; y < s.wW; y++ )
    {
        mvaddchu( s.hH - 1, y, '-' );
    }
    mvaddchu( s.hH - 1, s.hW - 1, ',' );

    s.lastC = lastC;
}

void drawCursorAs( const char * const str,
                   uint row,
                   uint col )
{
    DEBUG_ASSERT( strlen(str) == 6 );

    uint r;
    uint c;

    if ( (int) col != s.colOff )
    {
        cellWindowPos( row, col, &r, &c, ALIGN_LEFT, ALIGN_CENTER );
        mvaddchu( r, c, (uint) str[0] );
        /*
         if( (int)row != s.rowOff )
         {
         cellWindowPos( row, col, &r, &c, ALIGN_LEFT, ALIGN_LEFT );
         mvaddchu( r, c, (uint)str[2] );
         }

         cellWindowPos( row, col, &r, &c, ALIGN_LEFT, ALIGN_RIGHT );
         mvaddchu( r, c, (uint)str[3] );
         */
    }
    else
    {
        cellWindowPos( row, col, &r, &c, ALIGN_LEFT, ALIGN_RIGHT );
        mvaddchu( r, c + 1, (uint) str[1] );
    }
    /*
     if( (int)row != s.rowOff )
     {
     cellWindowPos( row, col, &r, &c, ALIGN_LEFT, ALIGN_LEFT );
     mvaddchu( r, c+1, (uint)str[1] );

     cellWindowPos( row, col, &r, &c, ALIGN_RIGHT, ALIGN_LEFT );
     mvaddchu( r, c, (uint)str[5] );
     mvaddchu( r, c-1, (uint)str[1] );
     }
     cellWindowPos( row, col, &r, &c, ALIGN_LEFT, ALIGN_RIGHT );
     mvaddchu( r, c+1, (uint)str[1] );

     cellWindowPos( row, col, &r, &c, ALIGN_RIGHT, ALIGN_CENTER );
     mvaddchu( r, c, (uint)str[0] );

     cellWindowPos( row, col, &r, &c, ALIGN_RIGHT, ALIGN_RIGHT );
     mvaddchu( r, c, (uint)str[4] );
     mvaddchu( r, c-1, (uint)str[1] );
     */
}

void drawCursor( void )
{
    drawCursorAs( "      ", (uint) s.prevRow, (uint) s.prevCol );
    drawCursorAs( "|-,`'.", (uint) s.curRow, (uint) s.curCol );

    s.prevRow = s.curRow;
    s.prevCol = s.curCol;
}

void drawFooter( void )
{
    char * curPos = curPos2Str( (uint) s.curRow, (uint) s.curCol );

    cleanArea( s.wH - s.fH, s.wW - s.fW, s.wH, s.wW );

    mvaddstr( (int )s.wH - 1, (int )s.wW - (int )strlen( curPos ), curPos );

    free( curPos );
}

void handleEvent( int k )
{
    for ( unsigned int i = 0; i < kListeners.size && cursesEnabled; i++ )
    {
        struct keyListener * l = getListener( i );
        if ( k == l->k ) (*l->callback)( k );
    }
}

void addSubToGroup( int k,
                    enum groupid g )
{
    pushBack( (struct list *) get( &subGroups, g ), newI( k ) );
}

void subKey( int k,
             void (*callback)( int ) )
{
    struct keyListener * l = malloc( sizeof(struct keyListener) );
    CHECK_ALLOC( l );

    l->k = k;
    l->callback = callback;

    pushBack( &kListeners, l );
}

void subGroup( enum groupid g,
               void (*callback)( int ) )
{
    struct list * groupList = (struct list *) get( &subGroups, g );
    for ( uint i = 0; i < groupList->size; i++ )
    {
        subKey( getI( groupList, i ), callback );
    }
}

void unsubKey( int k,
               void (*callback)( int ) )
{
    for ( unsigned int i = 0; i < kListeners.size; i++ )
    {
        struct keyListener * l = getListener( i );
        if ( l->k == k && l->callback == callback ) destroy( &kListeners, i-- );
    }
}

void unsubGroup( enum groupid g,
                 void (*callback)( int ) )
{
    struct list * groupList = (struct list *) get( &subGroups, g );
    for ( uint i = 0; i < groupList->size; i++ )
    {
        unsubKey( getI( groupList, i ), callback );
    }
}

void mvaddchu( const uint x,
               const uint y,
               const uint c )
{
    mvaddch( (int )x, (int )y, c );
}
