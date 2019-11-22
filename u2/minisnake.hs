{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

import Control.Monad (void)
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char

{- A data type for tokens, including the white tokens. Notably, TWhitespace
 - saves the length of the space. -}
data Tok
  = TInt Int
  | TVar String
  | TWhitespace Int
  | TNewLine
  deriving (Show, Eq, Ord)

isNewLine (T _ TNewLine) = True
isNewLine _ = False

showTok = show -- showTok is used below for printing the token errors; replace
               -- this implementation if you do not want stuff like 'TInt 5'
               -- showing up in error messages.


{- For nice error messages, we will need to reconstruct the original input
 - string from tokens later in the parsing process. That is not required, but
 - we can try anyway -- the T type wraps anything with a string that holds its
 - original textual representation. Some hypothetical tokens may be
 - represented as such:
 -
 - T "a" (TVar "a")
 - T "0x123" (TInt 291)
 - T "\"asd\"" (TString "asd")
 -}
data T a =
  T String
    a
  deriving (Show)

unT (T _ a) = a --drop T, leave only the contents

strT (T s _) = s --drop the contents and get the original string


{- FIRST-STAGE PARSING
 -
 - We define the tokenizer as something that just extracts tokens
 - (of type 'T Tok') from Strings. -}
type Tokenizer = Parsec Void String

{- The main token-parsing function -- you will want to add extra tokens here.
 -
 - We need to save the data twice (once as String for T, second in a parsed way
 - as a Tok), so we use Reader to avoid unnecessary manual copies. Note that
 - this code:
 -
 - (T <$> id <*> TVar) <$> some lowerChar
 -
 - ...is equivalent to somewhat longer:
 -
 - do str <- some lowerChar
 -    return T str (TVar str)
 -}
tok :: Tokenizer (T Tok)
tok =
  choice
    [ try ((T <$> id <*> TInt . read) <$> some digitChar)
    , (T <$> id <*> TVar) <$> some lowerChar
    , (T <$> id <*> TWhitespace . length) <$> some (char ' ')
    , T "\n" TNewLine <$ char '\n'
    ]

toks :: Tokenizer [T Tok]
toks = many tok

tokenize = runParser toks --this runs the parser


{- HIGH-LEVEL PARSING (a.k.a. second stage)
 -
 - This parsed language is simple, basically consists of whispace-separated
 - pairs of variables and numbers. Because we are not matching simple
 - characters anymore, we have to use more generic functions:
 -  - satisfy (matches the token if it satisfies a condition)
 -  - single (matches one exact token)
 -}
type Parser = Parsec Void [T Tok] --we are parsing a stream of tagged tokens now

pWhite :: Parser () --parse a single piece of white space
pWhite = (void (single TNewLine) <|> void (satisfy isWhitespace)) <?> "whitespace"
  where
    isWhitespace (TWhitespace _) = True
    isWhitespace _ = False

whitespace = void $ many (try pWhite) -- parse a lot of whitespace

pInt :: Parser Int -- match a single integer literal token
pInt = do
  TInt i <- satisfy isInt <?> "an integer"
  return i
  where
    isInt (TInt _) = True
    isInt _ = False

pVar :: Parser String -- match a single variable token
pVar = do
  TVar s <- satisfy isVar <?> "a variable name"
  return s
  where
    isVar (TVar _) = True
    isVar _ = False

pLexeme :: Parser a -> Parser a
pLexeme = (whitespace >>)

pair = (,) <$> pLexeme pVar <*> pLexeme pInt

pairs = many pair <* whitespace

parseExprs = runParser (pairs <* eof)


{- MAIN FUNCTION
 - 
 - Here, 'main' only shows how to run the parser now on a few expectable test
 - cases and print out the possible results or parser errors.
 -
 - In the final main, you will need to:
 - 1. get filename from commandline arguments
 - 2. read the string input from a file
 - 3. run the tokenizer
 - 4. convert some of the tokens into special tokens for marking the indent
 -    blocks, e.g. BlockStart and BlockEnd. For example, the code:
 -    
 -    a
 -      b
 -
 -    that now corresponds to tokens:
 -    [T "a" (Var "a"),
 -     T "\n" TNewLine,
 -     T "  " (TWhitespace 2),
 -     T "b"]
 -
 -    should be converted to something like:
 -
 -    [T "a" (Var "a"),
 -     T "\n" TNewLine,
 -     T "  " (TWhitespace 2),
 -     T "" TBlockStart,
 -     T "b",
 -     T "" TBlockEnd]
 - 5. run the parser on the list of tokens with block tokens added
 - 6. prettify the result using Text.PrettyPrint and print it out
 -}
main = do
  let Right tokens = tokenize "input.txt" "a 1 \n   b 5"
  putStrLn "*** Tokenizer output: ***"
  print tokens

  let Right exprs = parseExprs "input.txt" tokens
  putStrLn "\n*** Parser output: ***"
  print exprs
  
  let Left err =
        parseExprs
          "input.txt"
          ([T "123" (TInt 123), T " " (TWhitespace 1)] ++ tokens)
  putStrLn "\n*** Demo of an error message: ***"
  putStrLn $ errorBundlePretty err
  
  let Left err =
        parseExprs
          "input.txt"
          (tokens ++ [T " " (TWhitespace 1), T "123" (TInt 123)])
  putStrLn "*** Demo of an error message 2 (notice the line number!): ***"
  putStrLn $ errorBundlePretty err
  
  let Right tokens = tokenize "input.txt" "a \n\n\n 1  b b  b\n5"
  putStrLn "*** Tokens for error demo 3: ***"
  print tokens
  
  let Left err = parseExprs "input.txt" tokens
  putStrLn
    "*** Demo of an error message 3 (notice the line extracted from input): ***"
  putStrLn $ errorBundlePretty err


{- The rest of the code is an instance for Stream [T Tok], which serves as an
 - adaptor between lists of tagged tokens that we want to produce and
 - Megaparsec. There are similar adapters for String, Text and ByteString.
 -}
instance Stream [T Tok] where
  type Token [T Tok] = Tok -- type of a single token that the parsing functions will see
  type Tokens [T Tok] = [Tok] -- type of a token "chunk", basically a list of tokens
  tokenToChunk _ = (: []) -- some conversion functions between these ^^
  tokensToChunk _ = id
  chunkToTokens _ = id
  chunkLength _ = length
  chunkEmpty _ = null
  take1_ (x:xs) = Just (unT x, xs) -- this extracts the first token without the T tag
  take1_ _ = Nothing
  takeN_ n l@(_:_) = Just (map unT $ take n l, drop n l) -- same for 'n' tokens
  takeN_ _ _ = Nothing
  takeWhile_ f l = (map unT $ takeWhile (f . unT) l, dropWhile (f . unT) l)
  {- Now, let's put a very thick line here:
   -
   - *************************************************************************
   -
   - The rest of the instance is here only for the purpose of pretty error
   - printing -- it reconstructs the original code from the T tags and tells
   - Megaparsec about where the actual offending code is, so that it can
   - format an appropriate error message. It is completely unnecessary for the
   - assignment, but having nice parser errors messages that point to user code
   - (and not to a list of tokens) may help a lot. -}
  showTokens _ (a :| b) = intercalate ", " $ map showTok (a : b)
  reachOffset o pst =
    let oo = pstateOffset pst
        otoks = pstateInput pst
        lineEnds =
          filter (isNewLine . snd) $
          takeWhile ((<= o) . fst) $ zip [oo ..] otoks
        newo
          | null lineEnds = oo
          | otherwise = succ . fst . last $ lineEnds
        newtoks = drop (newo - oo) otoks
        line =
          case concatMap strT $ takeWhile (not . isNewLine) newtoks of
            "" -> "<empty line>"
            a -> convertTabs a
              where convertTabs = concatMap convertTab
                    convertTab '\t' = replicate (unPos $ pstateTabWidth pst) ' '
                    convertTab c = [c]
        sp = pstateSourcePos pst
        srcLine = mkPos $ unPos (sourceLine sp) + length lineEnds
        lineo = o - newo
        srcCol = mkPos . (+ 1) . length . concatMap strT . take lineo $ newtoks
        newSrcPos =
          SourcePos
            { sourceName = sourceName sp
            , sourceLine = srcLine
            , sourceColumn = srcCol
            }
     in ( newSrcPos -- NOTE: remove this 'newSrcPos' from the tuple if you have Megaparsec version >= 8.
        , line
        , pst
            { pstateInput = newtoks
            , pstateOffset = newo
            , pstateSourcePos = newSrcPos
            })
