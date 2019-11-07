
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
- statements (each statement consists of a single expression on a line)
- assignments (a single variable name, followed by `=` and an expression on a line)
- `if`, `if`+`else` and `while` statements, as in python (with colons)
- function definitions, as in python, but without default and keyword arguments.

Note that code blocks (as contained e.g. in the `if` statement) may be defined by significant whitespace indentation.

## Task 1

Use either `parsec` or `megaparsec` and the pretty-printing library `pretty` to
convert the (ugly) Slepýš code in a file specified by commandline argument to
equivalent prettified code, and print it on standard output.

- Add spaces around all operators, parentheses (ideally only on the outside),
  function calls (i.e. do not add space between the function name and the
  parenthesis with parameters), and similar constructions
- Add empty lines around definitions.
- Normalize the whitespace (remove duplicate or unnecessary padding and indentation)
- Wrap all code blocks in braces `{` and `}` and add semicolons after each
  statement and assignment as in C, so that the resulting code is not
  indentation-dependent (Note that because of the newly added braces and
  semicolons, the output of your program will _not_ be valid Slepýš code -- if
  you try to prettify it again using your program, it may reject the
  already-prettified version.)
- Reject invalid code (optionally with some good error message)

You can assume that there are _either_ only tab characters or only spaces used
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
  print(x + x);
}

i = 0;
j = read();
while i < j: {
  test(i);
  i = i + 1;
};
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
  c = b+d+e
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

Simplification: Do not consider recursion nor the "functions referring to functions defined later in the file"). E.g. this code:
```
def a():
  a()

a()
```
...may complain about undefined `a` in definition of `a`.

##### Unimportant side note

Slepýš programmers prefer using fixpoint combinator for recursion:

```
def a(rec):
  def f():
    rec()
  f

def fix(f):
  def fomega(x): f(x(x))
  fomega(fomega)

fix(a)()
```

# Hints

Use [getArgs](https://hackage.haskell.org/package/base-4.12.0.0/docs/System-Environment.html#v:getArgs) and optionally [exitWith](https://hackage.haskell.org/package/base-4.12.0.0/docs/System-Exit.html#v:exitWith) to get reasonable unix-ish behavior. Do not care about other commandline arguments (like the expectable `--help`). OTOH, make sure you handle the missing or unreadable file correctly (e.g. using some of the error-catching IO functions).

There are many ways to parse significant whitespace, but the most natural one is to first tokenize the source code, and add special tokens (e.g. `IndentIn` and `IndentOut`) to the places where indent width changes.

Notably, you may avoid some of the ambiguity by parsing the following code:

```
if a: b
      c
      d
```

as:

```
if a: {
  b;
};

{
  c;
  d;
};
```

## Hints and reminders about utility monads

### Parsers

`Parser a` from slides is not directly available in Parsec/Megaparsec, but you usually define the type from the types that you want for error handling and parsing:

```hs
type Parser = Parsec Void String
```

...says that there will be no error messages (`Void`) and the parser parses the items out of `String`. After that, e.g. `Parser Int` is exactly the type that parses an integer out of current context.

It is pretty customary to have 2 levels of parsing, first that creates tokens from string (`Parsec Void String [Token]`) and the second level that creates the AST from the list of tokens (`Parsec Void [Token] AST`). For parsing from strings, you will probably want to use the utility functions from [Text.Megaparsec.Char](https://hackage.haskell.org/package/megaparsec-7.0.5/docs/Text-Megaparsec-Char.html) or [Text.Megaparsec.Char.Lexer](https://hackage.haskell.org/package/megaparsec-7.0.5/docs/Text-Megaparsec-Char-Lexer.html) (or the corresponding varians for [Parsec](https://hackage.haskell.org/package/parsec-3.1.14.0)). For parsing custom token streams, you will probably need the [generic functions from here](https://hackage.haskell.org/package/megaparsec-7.0.5/docs/Text-Megaparsec.html#g:5).

This is not mandatory -- feel free to do both levels of parsing in one Parser.

### Reader&Writer monads

`Reader` is a monad that transparently [passes an environment around](https://hackage.haskell.org/package/mtl-2.2.2/docs/Control-Monad-Reader.html); consider it as a read-only `State`. You may want to use it for task 2, for transparently passing the variables defined in scope.

There are three main functions for working with `Reader` (the type annotations are simplified):

- `runReader :: Reader env a -> env -> a` executes the computation (in the first parameter) with global state of type `env` (second parameter) and returns whatever it ahs returned (of type `a`)
- in the computation, `ask :: Reader a a` reads and returns the global environment (similarly as `get` from `State`)
- `local :: (env -> env) -> Reader env a -> Reader env a` modifies the environment using the function from the first parameter, and runs a computation (second parameter) with the modified environment. The current computation then continues with the original environment.

For example:
```hs
f = do res <- local (*2) $ do a <- ask
                              return [a,a]
       return $ res++res

main = print $ runReader f 1
```
will print `[2,2,2,2]`.

`Writer` monad works the other way; it saves a write-only semigroup "state" that you can append to. The support functions are similar to Reader, except the most important ones are called `runWriter` and `tell`. It is great for collecting log messages for the error output of Task 2:

```
w = do tell ["ahoj"]
       tell ["log 2"]
       tell ["log 3"]
       return 42

main = print $ runWriter w
```

The program prints out `(42, ["ahoj","log 2","log 3"])`.

Combination of Reader and Writer monads that can do both of these things at once can be either simulated by State (this is a simple solution that everyone should be able to do), implemented manually (for some bonus points), re-used from the library implementation of Reader-Writer-State monad combination called [RWS](https://hackage.haskell.org/package/transformers-0.5.6.2/docs/Control-Monad-Trans-RWS-CPS.html#t:RWS) (for more bonus points!), OR, most correctly, combined from the two basic monads using a monad transformer, as `ReaderT env (Writer log) a` (for a vast amount of bonus points!!!).

Finally, N.B. there are 2 different monad libraries that implement both Reader, Writer and RWS: `mtl` and `transformers`. These differ by construction (the first one uses multiparameter type classes, the second one uses associated types), but from your point of view, the functionality should be roughly the same. If you can, use `transformers` for it is newer, cleaner and generally better.

# Submission

Create a cabal package named `yourSurname2` (just like in Homework 1).
Configure cabal that the package builds a single executable called
`slepys-format`. Use `cabal sdist` to obtain a redistributable, and submit to
the corresponding field in SIS as usual.

Before submission, check that the program can be executed using `cabal run`,
there are no reasonable deficiencies that would be reported by `hlint`, and the
code is at least as pretty as from `hindent`.
