
# Homework 1

Reminder: If you need a function for doing something that the standard library
should do, you can very probably find it on
[Hoogle](https://hoogle.haskell.org/).

## Optional assignment 0: a bit of I/O practice first

Write a program that computes average number of words (whitespace-separated
groups of characters) on each line in the standard input. As the output, write
only single number rounded to 2 decimal places.

You will want to use `getContents :: IO String` for reading the whole input.

For example, this input:

```
Ahoj nazdar
αχωι
```

should output something like this:

```
1.5
```

Try to shorten and compact the program as much as possible using Functor and/or
Applicative instances of the involved types. (Ideally, the solution could get
around 3x shorter than the text description above).

This assignment is completely optional, but you may get useful comments about
your code if you decide to submit. Also, the best (i.e. the shortest) submitted
solution will win some kind of symbolic award.

## Assignment 1 -- GameOfLife

[Brick](https://hackage.haskell.org/package/brick) is a library for building
terminal interfaces.

[Gloss](http://gloss.ouroborus.net/) is a library for building simple animated
graphical interfaces.

Use either of those for building a simple
[GameOfLife](https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life) cellular
automaton simulator.

Upon starting the program, the user should be presented with a board (at least
20x20 cells, add more if you wish) that can be controlled with following keys:

- arrows move a "cursor"
- pressing `x` changes the cell under the cursor (i.e. "flips" the cell state
  from empty to full or back)
- pressing Space moves the cellular automaton one step forward

There are no extra requirements, the main purpose of the assignment is to learn
working with the compiler, libraries and Cabal. In particular, do not worry
about using standard Haskell lists for arrays (although it is quite
inefficient) and use any simple form of graphics for displaying the output --
anything that is interactive and somehow readable by humans will do. Avoid
invalid operations that would crash the program, e.g. taking heads from empty
lists.

Additionally, there are example programs that you may use for getting a simple
base suitable for starting the solution:

- `brick.hs` (in this directory) contains a simple brick-based terminal
  interface that works with keys. There are some explanatory comments that
  should point you to the code that you want to modify.
- `gloss.hs` (also in this directory) contains a vaguely similar Gloss-based
  graphical interface. The comments are missing because most of the functions
  present are kindof self-explanatory.

You will need to install the dependency libraries before compiling or running
the programs directly -- use either
`cabal install brick`
or
`cabal install gloss`.

All functions used in the example programs are documented; find the
documentation using [Hoogle](https://hoogle.haskell.org/). Additionally, all Gloss-drawable picture components are [here](https://hackage.haskell.org/package/gloss-1.13.1.1/docs/Graphics-Gloss-Data-Picture.html); and some extra Brick widgets and layout tools are [here](https://hackage.haskell.org/package/brick-0.50/docs/Brick-Widgets-Core.html).

## How to submit

Haskell ecosystem recognizes standard packages that are created and managed
using [Cabal](https://www.haskell.org/cabal/users-guide/index.html). Cabal is
very similar to other packaging systems, such as npm, pip, dpkg, or even
autotools. We will discuss it more closely on the third lecture (alternatively,
you should be able to find a lot of good documentation and tutorials online, or
look at the examples in slides in advance).

You must pack your solution into a cabal package called `YourSurnameN` where
you substitute `YourSurname` for your surname, and `N` for the number of the
homework assignment (`0` or `1` in this case).

After you have the package working, use `cabal sdist` to obtain a source
distribution archive for the package (usually something like
`YourSurnameN.tar.gz`, or perhaps `YourSurnameN-0.1.0.0.tar.gz` with version
string). Upload this archive into the corresponding field in SIS study group
interface.

### Pre-submit checks

Before submitting, please verify the following:

1. The package can be compiled and installed on the unixes in the MFF computer
   lab.
2. `cabal run` executes the program.
2. Source code is formatted with `hindent` and still readable.
3. Source code is checked with `hlint` and all warnings or suggestions are
   either fixed or pointless.
