-- | This module provides the 'Fork' type and several fork instances.
-- A fork instance defines the branching of a node in the 'Tct.Core.ProofTree'.
module Tct.Core.Data.Forks
  (
  -- * Fork Instances
    Judgement (..)
  , judgement
  , Id (..)
  , toId, fromId
  , Pair (..)
  , Optional (..)
  ) where


import Control.Applicative (liftA2)

-- | 'Judgement' representst a node with no successor.
data Judgement a = Judgement deriving (Foldable,Functor,Traversable)

-- | Convenience function that ignores the Judgement.
--
-- prop> judgment a Judgment = a
judgement :: a -> Judgement b -> a
judgement a _ = a

-- | 'Id' represents a node with one successor.
newtype Id a = Id a
  deriving (Foldable, Functor, Traversable)

-- | 'Id' constructor.
toId :: a -> Id a
toId = Id

-- | 'Id' deconstructor.
fromId :: Id a -> a
fromId (Id c) = c

-- | 'Pair' represents a node with two successors.
newtype Pair a = Pair (a,a)

instance Functor Pair     where f `fmap` (Pair (a,b)) = Pair (f a, f b)
instance Foldable Pair    where foldr f e (Pair (a,b)) = a `f` (b `f` e)
instance Traversable Pair where f `traverse` (Pair (a,b)) = Pair <$> liftA2 (,) (f a) (f b)

-- | A 'Maybe' like type fork.
data Optional a b = Opt (a b) | Null  deriving (Foldable, Functor, Traversable)

