module Entry.Query where

data QueryTerm
  = Code String
  | Description String
  | Tag String
  | Language String
  deriving (Eq, Show)
