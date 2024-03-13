module Bencode.Encoder where

import Bencode.Value (BencodeValue (..))
import Control.Applicative
import Result

type Encoder a = a -> BencodeValue

dict :: Encoder v -> Encoder [(String, v)]
dict e vs = BencodeDict $ map (\(k, v) -> (k, e v)) vs

fields :: Encoder [(String, BencodeValue)]
fields = BencodeDict

list :: Encoder a -> Encoder [a]
list e vs = BencodeList $ map e vs

string :: Encoder String
string = BencodeString

int :: Encoder Int
int = BencodeInt
