module CharQq
(
  ord,
  ords,
  chr,
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
A quasi-quoter which produces a list of unicode codepoint integer literals of a sequence of chars.
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

{-|
A quasi-quoter which produces a char literal from a codepoint.
E.g.,

>>> [CharQq.chr|90|]
'Z'

Works in the context of expressions and patterns.
-}
chr :: QuasiQuoter
chr = QuasiQuoter exp pat typ dec where
  exp = Q.stringCodepointCharExp
  pat = Q.stringCodepointCharPat
  typ = const (fail "Unsupported")
  dec = const (fail "Unsupported")
