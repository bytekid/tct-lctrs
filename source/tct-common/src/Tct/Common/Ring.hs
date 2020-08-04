module Tct.Common.Ring 
  (
    module Tct.Core.Common.SemiRing
  , AdditiveGroup (..)
  , Ring 
  ) where


import Tct.Core.Common.SemiRing

-- | Extends 'Additive' to a additive group with inverse elements.
class Additive a => AdditiveGroup a where
  neg :: a -> a
  sub :: a -> a -> a
  sub a b = a `add` neg b

-- | 'Ring' instances should satisfy the 'SemiRing' laws:
-- Additionally:
--
-- * @'neg' a@ defines the inversible element of a
--
--    prop> a `add` neg a = zero
type Ring a = (AdditiveGroup a, Multiplicative a)

instance AdditiveGroup Int where
  neg = negate

