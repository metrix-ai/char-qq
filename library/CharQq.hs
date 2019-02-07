module CharQq
(
  ord,
  ords,
)
where

import CharQq.Prelude hiding (ord, chr)
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import qualified CharQq.Prelude as Prelude


-- * Quasi-quoters
-------------------------

{-|
A quasi-quoter which produces a unicode codepoint integer literal of a char.
E.g.,

>>> [CharQq.ord|Z|]
90

Works in the context of expressions and patterns.
-}
ord :: QuasiQuoter
ord = QuasiQuoter exp pat typ dec where
  exp = stringOrdExpQ
  pat = stringOrdPatQ
  typ = const (fail "Unsupported")
  dec = const (fail "Unsupported")

{-|
A quasi-quoter which produces a list of unicode codepoint integral literals of a sequence of chars.
E.g.,

>>> [CharQq.ords|абв|]
[1072,1073,1074]

Works in the context of expressions and patterns.
-}
ords :: QuasiQuoter
ords = QuasiQuoter exp pat typ dec where
  exp = stringOrdsExpQ
  pat = stringOrdsPatQ
  typ = const (fail "Unsupported")
  dec = const (fail "Unsupported")

-- * Q
-------------------------

stringOrdExpQ :: String -> Q Exp
stringOrdExpQ = \ case
  [char] -> return (LitE (IntegerL (fromIntegral (Prelude.ord char))))
  [] -> fail "Empty quotation"
  _ -> fail "Overlong quotation"

stringOrdPatQ :: String -> Q Pat
stringOrdPatQ = \ case
  [char] -> return (LitP (IntegerL (fromIntegral (Prelude.ord char))))
  [] -> fail "Empty quotation"
  _ -> fail "Overlong quotation"

stringOrdsExpQ :: String -> Q Exp
stringOrdsExpQ = return . ListE . map (LitE . IntegerL . fromIntegral . Prelude.ord)

stringOrdsPatQ :: String -> Q Pat
stringOrdsPatQ = return . ListP . map (LitP . IntegerL . fromIntegral . Prelude.ord)
