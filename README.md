# FPSheet - the prototype, written in C
## A Spreadsheet program with Haskell as the scripting language

![Alt text](imgs/example.png?raw=true "Example")

FPSheet is a spreadsheet program that runs inside a terminal.
The scripting language for computing cell's values from formulas runs Haskell internally.
This arguably provides a more uniform experience than what is provided by the scripting languages of standard spreadsheet programs. Note: this README currently assumes some familiarity from the reader with ghc's ghci tool.

Currently the tool starts a ghci session in the background. When a cell is defined or edited, a unique variable is defined or redefined inside this ghci session. For example, if cell ABD124 is edited to: "45", FPSheet sends the following to the ghci session: "let aBD124 = 45". Similarly, if cell A4 is edited to: "A5 * 10", FPSheet sends the following to the ghci session: "let a4 = a5 * 10".

Interestingly, since Haskell is lazily evaluated, and since Haskell regards function values as first class citizens, any custom function can be defined inside a cell. Any other cell can then apply this function simply by referring to the cell.

### Installation

dependencies: libncurses, libm, ghc

Additional dependency mention: libmpc (parser combinators in c), it is already present within the repo however with some minor type related edits.

run: make

### Usage

Run the fpsheet executable. (no interaction with arguments yet)

The tool currently always loads a spreadsheet instance from file ./.sheet (if this file is present, and is parseable). Similarly, currently upon exit the tool always saves the spreadsheet to ./.sheet.

The tool starts in navigation mode. This mode is comparable to vim's command mode. The arrow keys move the cursor over cells. The cursor can be moved straight to a certain cell by typing ":" followed by the cell's coordinate. In a similar fasion it is possible to move the cursor to a certain row (by typing ":" followed by the row number) and to move the cursor to a certain column (by typing ":" followed by the column name). The tool can be closed by typing ":q" in this mode (note that currently this is actually ":wq" as currently the tool always saves the state upon shutdown).

Cells with definitions starting with a ":" character are treated as ghci commands and are sent as is to ghci (whereas any other cell's definition is prepended with "let \<cell coordinate\> = ". This gives access to ghci's internals such as inferred types of cells (e.g. try out defining a cell as ":t \<some other cell\>").

Pressing "v" in navigation mode changes the mode to visual mode. Underlining of cells can be toggled in this mode by pressing "u". A double press of "d" in visual mode deletes the selected cell's content. Pressing ESCAPE exits visual mode.

### TODOs

- Rewrite in Haskell  (work in progress, see the master branch for its current state)
- Copy pasting cells (properly handling loose and stuck cell references)
- Reading and writing of user defined save files (i.e. any filename, not just ".sheet").
- Exporting to and importing from excell savefiles
- Undo & redo
- Many many more vital features (aka: TODO: write these out)

### WHY C?!?!

No idea
