module Bencode.Decoder where

import Bencode.Value
import Control.Applicative
import Result

data DecodeError = DecodeError

data Decoder a = Decoder {runDecoder :: BencodeValue -> Result DecodeError a}

dict :: Decoder a -> Decoder [(String, a)]
dict d = Decoder dec
  where
    dec (BencodeDict bvs) = sequenceA $ map decodeKV bvs
    dec _ = Error DecodeError

    decodeKV (k, v) = case runDecoder d v of
      Success a ->
        Success (k, a)
      Error e -> Error e

field :: String -> Decoder a -> Decoder a
field name d = Decoder dec
  where
    dec (BencodeDict bvs) = case lookup name bvs of
      Nothing -> Error DecodeError
      Just bv -> runDecoder d bv
    dec _ = Error DecodeError

list :: Decoder a -> Decoder [a]
list d = Decoder dec
  where
    dec (BencodeList bvs) = sequenceA $ map (runDecoder d) bvs
    dec _ = Error DecodeError

string :: Decoder String
string = Decoder dec
  where
    dec (BencodeString s) = Success s
    dec _ = Error DecodeError

int :: Decoder Int
int = Decoder dec
  where
    dec (BencodeInt i) = Success i
    dec _ = Error DecodeError

instance Functor Decoder where
  fmap f d = Decoder dec
    where
      dec bv =
        case runDecoder d bv of
          Success ok -> Success (f ok)
          Error e -> Error e

instance Applicative Decoder where
  pure f = Decoder $ \_ -> Success f
  f <*> d = Decoder dec
    where
      dec bv = case runDecoder f bv of
        Success fn -> runDecoder (fmap fn d) bv
        Error de -> Error de

instance Monad Decoder where
  return a = Decoder $ \_ -> Success a
  a >>= f = Decoder dec
    where
      dec bv = case runDecoder a bv of
        Success a' -> runDecoder (f a') bv
        Error de -> Error de

instance Alternative Decoder where
  empty = Decoder $ \_ -> Error DecodeError
  da <|> db = Decoder $ \bv ->
    case runDecoder da bv of
      sa@(Success _) -> sa
      Error errA -> case runDecoder db bv of
        sb@(Success _) -> sb
        Error errB -> Error DecodeError
