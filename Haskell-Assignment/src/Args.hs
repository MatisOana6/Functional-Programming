module Args where

import qualified Data.List as L
import Entry.Query
import Result
import Text.Read (readMaybe)

data AddOptions = AddOptions
  { addOptFilename :: String,
    addOptLanguage :: String,
    addOptDescription :: String,
    addOptTags :: [String]
  }
  deriving (Eq, Show)

data SearchOptions = SearchOptions
  { searchOptTerms :: [QueryTerm]
  }
  deriving (Eq, Show)

data GetOptions = GetOptions
  { getOptId :: Int
  }
  deriving (Eq, Show)

data Args
  = Add AddOptions
  | Search SearchOptions
  | Get GetOptions
  | Init
  | Help
  deriving (Show, Eq)

data ParseArgsError = NotEnoughArgs | InvalidArgs deriving (Show, Eq)

-- | Parse arguments of the `add` subcommand
parseAdd :: [String] -> Result ParseArgsError AddOptions
parseAdd (filename : language : description : tags) =
  Success
    AddOptions
      { addOptFilename = filename,
        addOptLanguage = language,
        addOptDescription = description,
        addOptTags = tags
      }
parseAdd _ = Error NotEnoughArgs

-- | Parse arguments of the `get` subcommand
parseGet :: [String] -> Result ParseArgsError GetOptions
parseGet [] = Error InvalidArgs
parseGet (id : _) = case (readMaybe id :: Maybe Int) of
  Nothing -> Error InvalidArgs
  Just id -> Success (GetOptions id)

-- | Parse a search term
--
-- >>> parseSearchTerm "code:main"
-- Success (Code "main")
--
-- >>> parseSearchTerm "desc:file"
-- Success (Description "file")
parseSearchTerm :: String -> Result ParseArgsError QueryTerm
parseSearchTerm term =
  case L.break (== ':') term of
    (field, ':' : term) ->
      case field of
        "code" -> Success $ Code term
        "desc" -> Success $ Description term
        "tag" -> Success $ Tag term
        "lang" -> Success $ Language term
        _ -> Error InvalidArgs
    _ -> Error InvalidArgs

-- | Parse arguments of the `search` subcommand
parseSearch :: [String] -> Result ParseArgsError SearchOptions
parseSearch [] = Error NotEnoughArgs
parseSearch terms = do
  searchTerms <- sequenceA $ map parseSearchTerm terms
  return $ SearchOptions searchTerms

-- | Parse the arguments of the program
--
-- >>> parseArgs ["add", "code.hs", "Haskell", "Some code"]
-- Success (Add (AddOptions {filename = "code.hs", language = "Haskell", description = "Some code", tags = []}))
--
-- >>> parseArgs ["search", "tag:io", "lang:haskell"]
-- Success (Search (SearchOptions {terms = [Tag "io",Language "haskell"]}))
--
-- >>> parseArgs ["get", "2"]
-- Success (Get (GetOptions {getOptId = 2}))
--
-- >>> parseArgs ["init"]
-- Success Init
parseArgs :: [String] -> Result ParseArgsError Args
parseArgs args
  | "-h" `elem` args || "--help" `elem` args = Success Help
parseArgs ("add" : rest) = Add <$> parseAdd rest
parseArgs ("search" : rest) = Search <$> parseSearch rest
parseArgs ("get" : rest) = Get <$> parseGet rest
parseArgs ("init" : rest) = Success Init
parseArgs _ = Error InvalidArgs
