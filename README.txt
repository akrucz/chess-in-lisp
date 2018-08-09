-----------
Description
-----------

This is small chess program (~750 lines or so) written using
Steel-Bank Common LISP.

It contains the core functionality of chess and a basic
implementation of a chess adversary (it was my first exposure
to the concept of an intelligent agent, so it's *very* primitive).

It is intended to be used as a command line script or loaded into
a command-line interpreter, etc., and the "ui.lisp" file provides
the functionality to print the board and prompt after every move.

----------
How to Use
----------

1)
In order to run this program, the constant "dirpath" in
"main.lisp" should be changed to whatever path you download
this package to.

2)
To begin a game, use the "play-chess" function:
The function is as follows:

(defun play-chess (&key (p1 'human)
                        (p2 'adversary)
                        (adversary-sym 'adversary)
                        (adversary-depth 0)
                        (automatic-game nil)
                        (p1lis '(♙ ♘ ♗ ♖ ♕ ♔))
                        (p2lis '(♟ ♞ ♝ ♜ ♛ ♚)))
...)


The easiest way to start a game is to simply call "(play-chess)" but if
one wishes to change some of the parameters here is some important info:

The p1 and p2 values cannot be the same.

If automatic-game is set to "t", then whatever is set to adversary-sym
gets ranked-moves and the other player gets random ones. If neither p1
nor p2 is set to the adversary-sym, then both p1 and p2 are considered
human players.

Adversary-depth is probably best left at 0, as anything more may take
too long to calculate with the current implementation.

One may change the p1lis and/or the p2lis to any other text characters (emoji too!)
they wish. Even all identical characters would work, since they
are only used when printing and do not change how the pieces move,
i.e. a pawn and a king may look the same but the program will still
perform the proper move-checking associated with each piece based on
their starting positions.

The input of a move in the prompt requires a particular form.
Examples of valid input moves are below:

  Pawn a2-A4
  pawnA2-a4
  Pa2-a4
  a2-a4
  a2a4


--------------------------
Potential Future Additions
--------------------------

- Castling
- En passant
- Minimax w/ alpha-beta pruning for the adversary
