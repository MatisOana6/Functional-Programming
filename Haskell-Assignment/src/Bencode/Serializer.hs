module Bencode.Serializer where

import Bencode.Value (BencodeValue (..))
import Text.Printf (printf)

newtype Serializer a = Serializer {runSerializer :: String -> a -> String}

serialize :: Serializer BencodeValue
serialize = Serializer ser
  where
    ser :: String -> BencodeValue -> String
    ser prev (BencodeInt i) = prev ++ (printf "i%de" i)
    ser prev (BencodeString s) = prev ++ (serStr s)
    ser prev (BencodeList l) = prev ++ printf "l%se" (foldl ser "" l)
    ser prev (BencodeDict d) = prev ++ printf "d%se" (foldl (\p (k, v) -> p ++ (serStr k) ++ (ser "" v)) "" d)

    serStr s = printf "%d:%s" (length s) s

-- >>> runSerializer serialize "" (BencodeInt 2)
-- "i2e"

-- >>> runSerializer serialize "i3e" (BencodeInt 2)
-- "i3ei2e"

-- >>> runSerializer serialize "" (BencodeList [])
-- "le"

-- >>> runSerializer serialize "" (BencodeList [BencodeInt 2, BencodeInt 1])
-- "li2ei1ee"

-- >>> runSerializer serialize "" (BencodeList [BencodeInt 2, BencodeString "abc"])
-- "li2e3:abce"

-- >>> runSerializer serialize "" $ BencodeDict [("k", BencodeInt 2)]
-- "d1:ki2ee"

-- >>> runSerializer serialize "" $ BencodeList [BencodeDict [("k", BencodeInt 2)], BencodeDict [("k", BencodeInt 3)]]
-- "ld1:ki2eed1:ki3eee"
