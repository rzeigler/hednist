module Data.Hednist.Types (
    Namespace,
    EDNValue(..)
  ) where

import Data.Set as S

type Namespace = String

-- TODO: Use an actual set for EDNValue, requires defining Ord on EDNValue...
-- I'm not certain if this is possible. I suspect that it is, but probably
-- requires interesting type algebra. In particular, EDNMap is challenging

data EDNValue = EDNNil
              | EDNBool Bool
              | EDNString String
              | EDNChar Char
              | EDNSymbol (Maybe Namespace) String
              | EDNKeyword (Maybe Namespace) String
              | EDNInt Int
              | EDNInteger Integer
              | EDNFloat Double
              | EDNList [EDNValue]
              | EDNVector [EDNValue]
              | EDNMap [(EDNValue, EDNValue)]
              | EDNSet [EDNValue]
              | EDNTagged (Maybe Namespace) String EDNValue
              | EDNDiscard EDNValue
                deriving (Eq, Show)

class ToEDN a where
  toEDN :: a -> EDNValue

class FromEDN a where
  fromEDN :: EDNValue -> Maybe a
