module CharQq.Pat
where

import CharQq.Prelude


codepoint :: Char -> Pat
codepoint = LitP . IntegerL . fromIntegral . ord

codepoints :: [Char] -> Pat
codepoints = ListP . map codepoint
