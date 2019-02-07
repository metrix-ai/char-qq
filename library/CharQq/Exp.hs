module CharQq.Exp
where

import CharQq.Prelude


codepoint :: Char -> Exp
codepoint = LitE . IntegerL . fromIntegral . ord

codepoints :: [Char] -> Exp
codepoints = ListE . map codepoint

char :: Char -> Exp
char = LitE . CharL
