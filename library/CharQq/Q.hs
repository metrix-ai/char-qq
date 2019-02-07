module CharQq.Q
where

import CharQq.Prelude
import qualified CharQq.Exp as Exp
import qualified CharQq.Pat as Pat


stringChar :: String -> Q Char
stringChar = \ case
  [char] -> return char
  [] -> fail "Empty quotation"
  _ -> fail "More than one char in the quotation"

stringCodepointExp :: String -> Q Exp
stringCodepointExp = fmap Exp.codepoint . stringChar

stringCodepointPat :: String -> Q Pat
stringCodepointPat = fmap Pat.codepoint . stringChar

stringCodepointsExp :: String -> Q Exp
stringCodepointsExp = return . Exp.codepoints

stringCodepointsPat :: String -> Q Pat
stringCodepointsPat = return . Pat.codepoints

codepointChar :: Int -> Q Char
codepointChar codepoint = if codepoint <= ord maxBound
  then return (chr codepoint)
  else fail "Codepoint is out of the supported Unicode range"

stringInt :: String -> Q Int
stringInt string = case readMaybe string of
  Just int -> return int
  Nothing -> fail "String doesn't parse to integer"

stringCodepointChar :: String -> Q Char
stringCodepointChar = stringInt >=> codepointChar

stringCodepointCharExp :: String -> Q Exp
stringCodepointCharExp = fmap Exp.char . stringCodepointChar

stringCodepointCharPat :: String -> Q Pat
stringCodepointCharPat = fmap Pat.char . stringCodepointChar
