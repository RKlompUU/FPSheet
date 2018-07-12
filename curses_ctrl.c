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
bool keepCursesRunning = false;

bool isCursesEnabled( void )
{
    return cursesEnabled;
}

void cleanArea( uint x1,
                uint y1,
                uint x2,
                uint y2 );

void cellWindowPos( uint r,
                    uint c,
                    uint * x,
                    uint * y,
                    enum alignment alignHor,
                    enum alignment alignVert );

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
    for ( int i = GROUP_SUB_NAVIG; i <= GROUP_SUB_CMD; i++ )
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
    keepCursesRunning = false;
}

void cursesCtrlLoop( void )
{
    keepCursesRunning = true;
    while ( keepCursesRunning )
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
    uint x1,y1,x2,y2;

    cellWindowPos( (uint)s.rowOff, (uint)s.colOff,
                   &x1, &y1,
                   ALIGN_LEFT,
                   ALIGN_LEFT );
    cellWindowPos( s.lastR+1, s.lastC+1,
                   &x2, &y2,
                   ALIGN_RIGHT,
                   ALIGN_RIGHT );

    cleanArea( x1, y1+1, x2-1, y2-1 );

    struct cell * cCurFocus = NULL;
    for ( unsigned int row = (uint)s.rowOff; row <= (uint)s.lastR; row++ )
    {
        for ( unsigned int col = (uint)s.colOff; col <= (uint)s.lastC; col++ )
        {
            struct cell * c = findCellP2( s.cells, row, col );
            if( c )
            {
                if( row == (uint)s.curRow &&
                    col == (uint)s.curCol )
                {
                    cCurFocus = c;
                    continue; // Draw currently focused cell last
                }
                drawCell( c, true );
            }
        }
    }

    if( cCurFocus )
    {
      drawCell( cCurFocus, false );
    }
}

void drawSheet()
{
    drawHeaders();
    drawFooter();

    render();
    drawCursor();

    switch ( s.mode )
    {
        case MODE_NAVIG:
        mvaddch( (int)s.wH - 1, 0, 'M' );
            break;
        case MODE_EDIT:
        mvaddch( s.wH - 1, 0, 'E' );
            break;
        case MODE_VISUAL:
        mvaddch( s.wH - 1, 0, 'V' );
            break;
        case MODE_COMMAND:
        mvaddstr( s.wH - 1, 0, s.cmd );
            break;
    }
}

void drawCell( const struct cell * const c, bool inBorders )
{
    uint x;
    uint y;
    cellWindowPos( c->p->row,// - (uint) s.rowOff,
                   c->p->col,// - (uint) s.colOff,
                   &x,
                   &y,
                   ALIGN_LEFT,
                   ALIGN_CENTER );
    uint n;
    if( inBorders )
    {
        n = s.cW;
    }
    else
    {
        n = s.wW-y-1;
    }

    if( c->res )
        mvaddnstr( (int)x, (int)y+1, c->res, (int)n );
    else
        mvaddnstr( (int)x, (int)y+1, c->txt, (int)n );

    if( c->bar )
    {
        int length = s.cW;
        struct cell * cRight = findCellP2( s.cells, c->p->row, c->p->col+1 );
        if( cRight && cRight->bar )
            length++;
        mvchgat( (int)x, (int)y+1, length, A_UNDERLINE, 0, NULL );
    }
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
        uint bcH = s.cH + s.bH;
        *x = hH + (r - (uint) s.rowOff) * bcH + 1;

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
        uint bcW = s.cW + s.bW;
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

    uint bcH = s.cH + s.bH; // Bordered cell height
    uint lastR = (uint) s.rowOff + (s.wH - s.hH - bcH - (s.bH == 0 ? 1 : 1)) / bcH; // - bcH, - 1 to prevent a possible half-row
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

    uint bcW = s.cW + s.bW;
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
    }
    else
    {
        cellWindowPos( row, col, &r, &c, ALIGN_LEFT, ALIGN_RIGHT );
        mvaddchu( r, c + 1, (uint) str[1] );
    }

    if( s.mode == MODE_EDIT )
    {
        cellWindowPos( row, col, &r, &c, ALIGN_LEFT, ALIGN_RIGHT );
        mvchgat( (int)r-1, (int)c+1+s.editCursor, 1, A_UNDERLINE, 0, NULL );
    }
}

void drawCursor( void )
{
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
    if( k == 27 )
    {
        k = KEY_ESC;
        /*
        nodelay( stdscr, true );
        int i = getch();
        if( i == ERR || i == 27 )
            k = KEY_ESC;
        else
        {
            k = KEY_ALT;
            ungetch( i );
        }
        nodelay( stdscr, false );
        */
    }
    for ( unsigned int i = 0; i < kListeners.size && cursesEnabled; i++ )
    {
        struct keyListener * l = getListener( i );
        if ( k == l->k )
            (*l->callback)( k );
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
