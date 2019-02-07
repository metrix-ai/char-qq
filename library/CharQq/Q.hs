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
