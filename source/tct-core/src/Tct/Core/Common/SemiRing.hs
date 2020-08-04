-- | This module provides the abstract 'SemiRing' and 'Ring' type.
module Tct.Core.Common.SemiRing
  (
  Additive (..)
  , Multiplicative (..)
  , SemiRing

  -- * Foldable Versions
  , bigAdd
  , bigMul
  ) where


import qualified Data.Foldable as F (Foldable, foldl)


-- | The commutative monoid underlying a 'SemiRing'.
class Additive a where
  add  :: a -> a -> a
  zero :: a

-- | The monoid underlying a 'SemiRing'.
class Multiplicative a where
  mul :: a -> a -> a
  one :: a

-- | The 'Additive' and 'muliplicative' instances
-- should satisfy the following laws:
--
-- * (a,'add') is a commutative monoid with identity 'zero':
--
--     prop> a `add` (b `add` c) = (a `add` b) `add` c
--     prop> a `add` b = b `add` a
--     prop> zero `add` a = a `add` zero = a
--
-- * (a,'mul') is a monoid with identity 'one':
--
--     prop> a `mul` (b `mul` c) = (a `mul` b) `mul` c
--     prop> one `mul` a = a `mul` one = a
--
-- * 'mul' left and right distributes over 'add':
--
--     prop> a `mul` (b `add` c) = (a `mul` b) `add` (a `mul` c)
--     prop> (a `add` b) `mul` c = (a `mul` c) `add` (b `mul` c)
--
-- * 'mul' by 'zero' annihilates a
--
--     prop> zero `mul` a = a `mul` zero = zero
type SemiRing a = (Additive a, Multiplicative a)


-- | Foldable version of 'add'.
bigAdd :: (F.Foldable t, Additive a) => t a -> a
bigAdd = F.foldl add zero

-- | Foldable version of 'mul'.
bigMul :: (F.Foldable t, Multiplicative a) => t a -> a
bigMul = F.foldl mul one


-- int instance
instance Additive Int where
  zero = 0
  add  = (+)

instance Additive Bool where
  zero = False
  add  = (||)

instance Multiplicative Int where
  one  = 1
  mul = (*)

instance Multiplicative Bool where
  one = True
  mul = (&&)


