module CharQq
(
  ord,
  ords,
)
where

import CharQq.Prelude hiding (ord, chr)
import qualified CharQq.Q as Q


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
  exp = Q.stringCodepointExp
  pat = Q.stringCodepointPat
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
  exp = Q.stringCodepointsExp
  pat = Q.stringCodepointsPat
  typ = const (fail "Unsupported")
  dec = const (fail "Unsupported")
