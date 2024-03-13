module Parsec where

import qualified Data.List as L
import Result
import Text.Printf (printf)

data ParseError
  = UnexpectedEndOfInput
  | UnexpectedInput {gotInput :: String, expectedInput :: String}
  | Other String
  deriving (Show, Eq)

isUnexpectedEndOfInput :: ParseError -> Bool
isUnexpectedEndOfInput UnexpectedEndOfInput = True
isUnexpectedEndOfInput _ = False

isUnexpectedInput :: ParseError -> Bool
isUnexpectedInput (UnexpectedInput _ _) = True
isUnexpectedInput _ = False

isOther :: ParseError -> Bool
isOther (Other _) = True
isOther _ = False

isTodo :: ParseError -> Bool
isTodo (Other "TODO") = True
isTodo _ = False

type ParseResult a = Result ParseError (a, String)

data Parser a = Parser {runParser :: String -> ParseResult a, label :: String}

success = curry Success

parser input = Parser input ""

expecting :: Parser a -> String -> Parser a
expecting parser msg = Parser inner msg
  where
    inner input = case (runParser parser input) of
      Success ok -> Success ok
      Error err -> Error $ UnexpectedInput input msg

-- Base parsers -------------------------------------------------------

satisfies :: (Char -> Bool) -> String -> Parser Char
satisfies predicate msg = Parser inner msg
  where
    inner "" = Error UnexpectedEndOfInput
    inner (first : rest) =
      if predicate first
        then success first rest
        else Error (UnexpectedInput [first] msg)

lower :: Parser Char
lower = satisfies (\c -> elem c ['a' .. 'z']) "lowercase character"

upper :: Parser Char
upper = satisfies (\c -> elem c ['A' .. 'Z']) "uppercase character"

char :: Char -> Parser Char
char c = satisfies (== c) ("character " ++ show c)

digit :: Parser Char
digit = satisfies (`elem` ['0' .. '9']) "digit"

letter :: Parser Char
letter = lower `orElse` upper `expecting` "letter"

-- | Parser that always succeeds with a given value, consuming no input
succeed :: a -> Parser a
succeed a = parser $ \input -> Success (a, input)

-- | Parser that always fails with a given message
fail :: String -> Parser a
fail err = Parser (\input -> Error (Other err)) err

-- Combinators ---------------------------------------------------------

-- | Chain two parses, running the second parser with the remaining input from the first parser
-- >>> runParser (andThen (string "abc") number) "abc1234"
-- Success (("abc",1234),"")
--
-- >>> runParser (andThen (string "abc") number) "abcd1234"
-- Error (UnexpectedInput {gotInput = "d1234", expectedInput = "number"})
--
-- >>> runParser (andThen (string "abc") number) "abcd1234"
-- Error (UnexpectedInput {gotInput = "d1234", expectedInput = "number"})
andThen :: Parser a -> Parser b -> Parser (a, b)
andThen pa pb = parser inner
  where
    inner input =
      case runParser pa input of
        Success (a, rest) ->
          case runParser pb rest of
            Success (b, remaining) -> success (a, b) remaining
            Error err -> Error err
        Error err -> Error err

-- | Chain two parses, discarding the value of the first parser
pThen :: Parser a -> Parser b -> Parser b
pThen pa pb = parser inner
  where
    inner input =
      case runParser pa input of
        Success (_, rest) -> runParser pb rest
        Error err -> Error err

-- | Try to run the first the parser, if it succeeds, return the result, otherwise run the second parser
--
-- >>> runParser (orElse (string "abc") (string "efg")) "def"
-- Error (UnexpectedInput {gotInput = "def", expectedInput = "string \"abc\" or string \"efg\""})
orElse :: Parser a -> Parser a -> Parser a
orElse pa pb = Parser inner msg
  where
    msg = printf "%s or %s" (label pa) (label pb)
    inner input =
      case runParser pa input of
        Success (a, rest) -> success a rest
        Error _ -> case runParser pb input of
          Error _ -> Error (UnexpectedInput input msg)
          Success (ok, rest') -> success ok rest'

-- | Run the parser while it succeeds 0 or more times, collecting the results in a list.
--
--  Never fails!
many :: Parser a -> Parser [a]
many p = parser inner
  where
    inner "" = success [] ""
    inner input =
      case runParser p input of
        Success (r, rest) ->
          -- `many` always succeeds
          case runParser (many p) rest of
            Success (rs, remaining) -> success (r : rs) remaining
        Error _ -> success [] input

-- | Run the parser while it succeeds 1 or more times, collecting the results in a list.
--
--  Fails if the parser doesn't succeed at least once.
--
-- >>> runParser (some (char 'a')) "aa"
-- Success ("aa","")
--
-- >>> runParser (some (char 'a')) ""
-- Error UnexpectedEndOfInput
--
-- >>> runParser (some (char 'a')) "bb"
-- (UnexpectedInput {gotInput = "bb", expectedInput = "At least one character 'a'"})
some :: Parser a -> Parser [a]
some p = Parser inner msg
  where
    msg = printf "At least one %s" (label p)
    inner "" = Error UnexpectedEndOfInput
    inner input =
      case runParser p input of
        Success (r, rest) ->
          -- `many` always succeeds
          case runParser (many p) rest of
            Success (rs, remaining) -> success (r : rs) remaining
        Error err -> Error (UnexpectedInput input msg)

-- | Run a parser, if it succeeds, return the result wrapped in Just, else return Nothing.
--
--  Always succeeds.
try :: Parser a -> Parser (Maybe a)
try p = Parser inner (label p)
  where
    inner input =
      case runParser p input of
        Success (r, rest) -> success (Just r) rest
        Error err -> success Nothing input

-- Transforming and chaining ----------------------------------------------------

-- | Transform the result of a parser.
pMap :: (a -> b) -> Parser a -> Parser b
pMap f p = Parser inner (label p)
  where
    inner "" = Error UnexpectedEndOfInput
    inner input =
      case runParser p input of
        Success (r, rest) -> success (f r) rest
        Error err -> Error err

-- | Chain two parsers, feeding both the result and the remaining input from the first parser to the second parser.
with :: Parser a -> (a -> Parser b) -> Parser b
with pa f = parser inner
  where
    inner input =
      case runParser pa input of
        Success (a, rest) ->
          case runParser (f a) rest of
            Success (b, remaining) -> success b remaining
            Error err -> Error err
        Error err -> Error err

-- Convenience functions -----------------------------------------------------------------------------------------------

-- | Parse a number
number :: Parser Int
number = pMap read (some digit) `expecting` "number"

-- | Parse a string.
string :: String -> Parser String
string "" = parser (\input -> success "" input)
string (c : cs) = pMap (\(x, xs) -> x : xs) (andThen (char c) (string cs)) `expecting` (printf "string \"%s\"" (c : cs))

-- | Run a parser between two other parsers, discarding the result of the enclosing parsers
--
-- >>> runParser (between (char '"') (char '"') (many letter)) "\"Hello\""
-- Success ("Hello","")
-- >>> runParser (between (char '[') (char ']') number) "[1]"
-- Success (1,"")
--
-- >>> runParser (between (char '[') (char ']') number) "[1|"
-- Error (UnexpectedInput {gotInput = "|", expectedInput = "character ']'"})
between :: Parser a -> Parser b -> Parser c -> Parser c
between pHd pTl p = pHd `pThen` pMap fst (p `andThen` pTl)

-- | Parser that consumes a fixed number of characters.
take :: Int -> Parser String
take nr = parser $ \input ->
  uncurry success (L.splitAt nr input)
