# FPSheet
## A Spreadsheet program with Haskell as the scripting language

The prototype was written in C, it can be tried out in the c-prototype branch.
I am currently rewriting the tool in Haskell, which I am doing in this master branch.

![Alt text](imgs/example.png?raw=true "Example")

FPSheet is a spreadsheet program, where the scripting language for computing cell's values from formulas runs Haskell internally.
This arguably provides a more uniform experience than what is provided by the scripting languages of standard spreadsheet programs. Note: this README currently assumes some familiarity from the reader with ghc's ghci tool.

Currently the tool starts a ghci session in the background. When a cell is defined or edited, a unique variable is defined or redefined inside this ghci session. For example, if cell ABD124 is edited to: "45", FPSheet sends the following to the ghci session: "let aBD124 = 45". Similarly, if cell A4 is edited to: "A5 * 10", FPSheet sends the following to the ghci session: "let a4 = a5 * 10".

Interestingly, since Haskell is lazily evaluated, and since Haskell regards function values as first class citizens, any custom function can be defined inside a cell. Any other cell can then apply this function simply by referring to the cell.

### Installation

Not applicable yet in this Haskell rewrite.

### Usage

Not applicable yet in this Haskell rewrite.

### TODOs

- Rewrite in Haskell  (work in progress)
- Copy pasting cells (properly handling loose and stuck cell references)
- Reading and writing of user defined save files (i.e. any filename, not just ".sheet").
- Exporting to and importing from excell savefiles
- Undo & redo
- Many many more vital features (aka: TODO: write these out)