module Bencode.Value where

type BencodeKW = (String, BencodeValue)

data BencodeValue
  = BencodeInt Int
  | BencodeString String
  | BencodeList [BencodeValue]
  | BencodeDict [BencodeKW]
  deriving (Show, Eq)
