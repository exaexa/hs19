
# Homework 2 -- Prettification

Reminder: This is about applicatives and monads. Look in the slides for inspiration.

## Background

On the lectures, we have talked about monadic parsers, e.g. the ones from packages `parsec` and `megaparsec`. These allow very natural constructions of powerful, fast parsing libraries.

We will also talk about text processing, which includes pretty-printing.

You can find documentation on parsing in the slides; there are good tutorials with a lot of example code around. Some of them are [listed here](https://markkarpov.com/learn-haskell.html#megaparsec-tutorials), one that matches our purpose most closely is probably [this one](https://markkarpov.com/megaparsec/parsing-simple-imperative-language.html). Refer to [Megaparsec documentation](https://hackage.haskell.org/package/megaparsec) for more details.

Pretty-printing package documentation is [available as well](https://hackage.haskell.org/package/pretty), but it is perhaps best documented by a simple example:

```hs
import Text.PrettyPrint

-- a simple representation of Scheme language
data Scheme
  = Ident String
  | Number Int
  | Seq [Scheme]

-- class for converting anything PDoc-ish to Doc (which PrettyPrint works with)
class PDoc a where
  pdoc :: a -> Doc

-- converts anything PDoc-ish to a Doc and renders it to a string using PrettyPrint
ppshow :: PDoc a => a -> String
ppshow = renderStyle (style {lineLength = 80}) . pdoc

-- Scheme is in PDoc, i.e. can be prettyprinted
instance PDoc Scheme where
  pdoc (Ident a) = text a
  pdoc (Number i) = int i
  pdoc (Seq []) = parens empty
  pdoc (Seq (x:xs)) = parens $ pdoc x <+> sep (map pdoc xs)

-- example Scheme AST generators
short k = Seq $ Ident "*" : map Number [k .. k + 3]

long = Seq $ Ident "+" : map short [1 .. 10]

-- usage
main = putStrLn . ppshow $ Seq [Ident "factorial", long]
```

## Task setup

The language Slepýš (in Slovak the language is called Slepúch, in
English-speaking countries it would probably be called Blindworm) allows the
following constructions:

- expressions, formed from numeric literals (like `-1`, `0` or `0123`) and
  string literals (`"asad"` or `"asda\nasda"`), variable names consisting of
  characters `a`-`z`, `A`-`Z` and `_`, and combinations thereof using binary
  operators `+`, `-`, `*`, `/` and parentheses
- function calls formatted using parentheses and commas as usual, e.g.
  `asd(1,2,345)`
- statements (a single expressions on a line)
- assignments (a single variable name, followed by `=` and an expression on a line)
- `if`, `if`+`else` and `while` statements, as in python (with colons)
- function definitions, as in python, but without default and keyword arguments.

Note that code blocks (as contained e.g. in the `if` statement) may be defined by significant whitespace indentation.

## Task 1

Use either `parsec` or `megaparsec` and the pretty-printing library `pretty` to
convert the (ugly) Slepýš code in a file specified by commandline argument.

- Add spaces around all operators, parentheses (ideally only on the outside),
  function calls (i.e. do not add space between the function name and the
  parenthesis with parameters), and similar constructions
- Add empty lines around definitions.
- Normalize the whitespace (remove duplicate or unnecessary padding and indentation)
- Wrap all code blocks in braces `{` and `}` and add semicolons after each
  statement and assignment as in C, so that the resulting code is not
  indentation-dependent
- Reject invalid code (optionally with some good error message)

You can assume that there are either only tab characters or only spaces used
for indentation (i.e. print an error if you find any of the other kind). If
possible, prefer spaces.

For example, a file `test.slepys`:

```
def test(x): print(x+x)
i=0
j=read()
while i<j:
   test(i)
   i=i+1
```

can be processed by your program as such:

```
slepys-format test.slepys
```

The output may look as follows:
```
def test(x): {
  print(x + x)
}

i = 0
j = read()
while i < j: {
  test(i)
  i = i + 1
}
```

Conversely, every line of this code should be rejected for many different reasons:

```
def a:
def ():
123asd = asd321 + + - 321
else: pass
if < a: xxx
```

## Task 2

After parsing the code, check whether all used variables have been previously
defined (i.e. the identifier was either captured as a function parameter,
assigned in some previous expression, or stands for a name of some defined
function). If not, print out an error message with names of all undeclared
identifiers and the parts of code where they are located. Identifiers `print`,
`read` and `pass` (with Python-like semantics) are in the "standard library"
and get defined automatically on the beginning of the code.

For example, checking the following code:

```
d = "asd"
def a(b):
  c = b + d + e
print(a(c))
```

should produce an error report like this:

```
Error: Undeclared identifier 'e'
  .. found in assignment
     'c = b + d + e'
  .. found in definition of 'a'
Undeclared identifier 'c'
  .. found in statement
     'print(a(c))'
```

Hint: Use some combination of Reader, Writer or State monad to avoid passing
the environment around, and carrying error log out of the code manually.

# Submission

Name the project `slepys-format`, pack it in a Cabal package as usual, and
submit to the corresponding field in SIS.

Before submission, check that the program can be executed using `cabal run`,
there are no reasonable deficiencies that would be reported by `hlint`, and the
code is at least as pretty as from `hindent`.
