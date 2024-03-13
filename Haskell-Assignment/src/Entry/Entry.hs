module Entry.Entry where

import qualified Bencode.Decoder as De
import qualified Bencode.Encoder as Enc
import qualified Bencode.Parser as Ben
import qualified Data.Char as Char
import Data.Function (on)
import Data.List (intercalate)
import qualified Data.List as L
import Entry.Query (QueryTerm (..))
import Result
import Text.Printf (printf)

data Entry = Entry
  { entryId :: Int,
    entrySnippet :: String,
    entryFilename :: String,
    entryLanguage :: String,
    entryDescription :: String,
    entryTags :: [String]
  }
  deriving (Eq, Show)

newtype FmtEntry = FmtEntry Entry

instance Show FmtEntry where
  show (FmtEntry entry) =
    printf
      "[%d] %s:\n   Description: %s\n   Tags: %s\n   First line: %s"
      (entryId entry)
      (entryFilename entry)
      (entryDescription entry)
      (unwords $ entryTags entry)
      (head $ lines (entrySnippet entry))

entryEncoder :: Enc.Encoder Entry
entryEncoder e =
  Enc.fields
    [ ("id", Enc.int (entryId e)),
      ("snippet", Enc.string (entrySnippet e)),
      ("filename", Enc.string (entryFilename e)),
      ("language", Enc.string (entryLanguage e)),
      ("description", Enc.string (entryDescription e)),
      ("tags", Enc.list Enc.string (entryTags e))
    ]

entryListEncoder :: Enc.Encoder [Entry]
entryListEncoder es = Enc.list entryEncoder es

entryDecoder :: De.Decoder Entry
entryDecoder =
  Entry
    <$> (De.field "id" De.int)
    <*> (De.field "snippet" De.string)
    <*> (De.field "filename" De.string)
    <*> (De.field "language" De.string)
    <*> (De.field "description" De.string)
    <*> (De.field "tags" (De.list De.string))

entryListDecoder :: De.Decoder [Entry]
entryListDecoder = De.list entryDecoder

matchedByQuery :: QueryTerm -> Entry -> Bool
matchedByQuery (Code s) entry = (L.isInfixOf `on` toLower) s (entrySnippet entry)
matchedByQuery (Description s) entry = (L.isInfixOf `on` toLower) s (entryDescription entry)
matchedByQuery (Tag s) entry = (toLower s) `elem` (map toLower (entryTags entry))
matchedByQuery (Language s) entry = ((==) `on` toLower) s (entryLanguage entry)

-- | Check if an entry is matched by all queries
matchedByAllQueries :: [QueryTerm] -> Entry -> Bool
matchedByAllQueries qs entry =
  and $ queries <*> pure entry
  where
    queries = map matchedByQuery qs

toLower :: String -> String
toLower = map Char.toLower

-- >>> matchedByQuery (Code "+") (Entry 1 "x + 1" "add.hs" "Haskell" "Add 2 numbers" ["math", "simple"])
-- True

-- >>> matchedByQuery (Tag "math") (Entry 1 "x + 1" "add.hs" "Haskell" "Add 2 numbers" ["math", "simple"])
-- True

-- >>> matchedByQuery (Tag "io") (Entry 1 "x + 1" "add.hs" "Haskell" "Add 2 numbers" ["math", "simple"])
-- False

-- >>> matchedByQuery (Description "add") (Entry 1 "x + 1" "add.hs" "Haskell" "Add 2 numbers" ["math", "simple"])
-- True
