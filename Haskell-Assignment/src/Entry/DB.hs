module Entry.DB (LoadDBError (..), SaveError (..), SnippetDB, empty, load, save, modify, findFirst, findAll, insertWith, serialize, fromEntries) where

import qualified Bencode.Decoder as De
import qualified Bencode.Encoder as Enc
import qualified Bencode.Parser as Ben
import qualified Bencode.Serializer as Ser
import Bencode.Value
import qualified Data.List as L
import Entry.Entry
import Result
import Test.SimpleTest.Mock (TestableMonadIO (readFile, writeFile))
import Prelude hiding (readFile, writeFile)

data LoadDBError
  = FileNotFound
  | CorruptedFile
  | InvalidStructure
  deriving (Show, Eq)

data SaveError = SaveError deriving (Show, Eq)

data UpdateError = UpdateError deriving (Show, Eq)

data SnippetDB = SnippetDB [Entry] deriving (Show, Eq)

empty :: SnippetDB
empty = SnippetDB []

fromEntries :: [Entry] -> SnippetDB
fromEntries entries = SnippetDB entries

-- | Eagerly read a file from disk.
readFile' :: TestableMonadIO m => String -> m String
readFile' filename = do
  contents <- readFile filename
  return $ length contents `seq` contents

-- | Eagerly write a file to disk.
writeFile' :: TestableMonadIO m => String -> String -> m ()
writeFile' filename contents =
  length contents `seq` writeFile filename contents

-- | Load the database from disk.
load :: TestableMonadIO m => m (Result LoadDBError SnippetDB)
load = do
  contents <- readFile' "snippets.ben"
  return $ case Ben.parse contents of
    Error pe -> Error CorruptedFile
    Success bv -> buildEntries bv
      where
        buildEntries :: BencodeValue -> Result LoadDBError SnippetDB
        buildEntries bv =
          case De.runDecoder entryListDecoder bv of
            Success es -> Success (SnippetDB es)
            Error de -> Error InvalidStructure

-- | Serialize the database to a string.
serialize :: SnippetDB -> String
serialize (SnippetDB db) =
  Ser.runSerializer Ser.serialize "" bv
  where
    bv = entryListEncoder db

-- | Save the given database to disk, overwriting the previous database.
save :: TestableMonadIO m => SnippetDB -> m (Result SaveError ())
save db = Success <$> writeFile' "snippets.ben" (serialize db)

-- | Load the database, modify the database using the provided function and then save it.
modify :: TestableMonadIO m => (SnippetDB -> SnippetDB) -> m (Result UpdateError ())
modify fn = do
  db <- load
  case db of
    (Error lde) -> return $ Error UpdateError
    (Success db) -> do
      let db' = fn db
      mapError (const UpdateError) <$> save db'

-- | Find the first matching entry in the database.
findFirst :: (Entry -> Bool) -> SnippetDB -> Maybe Entry
findFirst p (SnippetDB entries) =
  L.find p entries

-- | Get all matching entries from the database.
findAll :: (Entry -> Bool) -> SnippetDB -> [Entry]
findAll p (SnippetDB entries) =
  filter p entries

-- | Insert an entry in the database. The id for the entry is provided by the first parameter of the lambda.
--
-- >>> insertWith (\id -> Entry id "x + 1" "add.hs" "Haskell" "Add 2 numbers" ["math", "simple"]) empty
-- SnippetDB [Entry {entryId = 0, entrySnippet = "x + 1", entryFilename = "add.hs", entryLanguage = "Haskell", entryDescription = "Add 2 numbers", entryTags = ["math","simple"]}]
insertWith :: (Int -> Entry) -> SnippetDB -> SnippetDB
insertWith genEnt db@(SnippetDB entries) = SnippetDB (ent : entries)
  where
    ent = genEnt (getNextId db)
    getNextId (SnippetDB entries) = length entries
