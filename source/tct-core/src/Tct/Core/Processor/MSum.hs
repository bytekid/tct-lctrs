{-# LANGUAGE MultiWayIf #-}
-- | This module provides the /MSum/ processor.
--
-- The /MSum/ processor runs two strategies in parallel and provides a "sum" on the resulting proof trees and
-- certificates. Informally, the processor behaves like a monoid where open (sub)prooftrees and unknown bounds behave
-- like neutral elements in the proof construction. Closed prooftrees are integrated as left and right child. The
-- certificates are combined, that is `min` for upper bounds and `max` for lower bounds and `Unknown` complexity is
-- treated as a neutral element.
--
-- Motivating usage: msum lower_bound_strategy upper_bound_strategy
module Tct.Core.Processor.MSum
  ( madd
  , maddDeclaration)
  where

import Tct.Core.Data

import Tct.Core.Processor.Failing (close)


data Gabel a = None | One a | Two a a
  deriving (Functor, Foldable, Traversable)

data Sum i o = Sum
  { left  :: Strategy i o
  , right :: Strategy i o
  } deriving Show


bounded :: Certificate -> Bool
bounded = not . isUnbounded

succeed :: (ProofObject p ~ (), Forking p ~ Gabel, Monad m) => Gabel (ProofTree (Out p)) -> m (Return p)
succeed po = return $ Progress () certf po where

  certf None        = unbounded
  certf (One c)     = c
  certf (Two c1 c2) = Certificate
    { spaceUB = spaceUB c1 `min` spaceUB c2
    , spaceLB = spaceLB c1 `maz` spaceLB c2
    , timeUB  = timeUB c1  `min` timeUB c2
    , timeLB  = timeLB c1  `maz` timeLB c2 }

  Unknown `maz` b       = b
  b       `maz` Unknown = b
  a       `maz` b       = a `max` b


instance (ProofData i, ProofData o) => Processor (Sum i o) where
  type ProofObject (Sum i o) = ()
  type In  (Sum i o)         = i
  type Out (Sum i o)         = o
  type Forking (Sum i o)     = Gabel

  execute p prob = do
    let k pr = evaluate (timeoutRemaining $ pr p) (Open prob)
    (lpt,rpt) <- concurrently (k left) (k right)
    let
      lc = certificate lpt
      rc = certificate rpt

    if
      | bounded lc && bounded rc -> succeed $ Two lpt rpt
      | bounded lc               -> succeed $ One lpt
      | bounded rc               -> succeed $ One rpt
      | otherwise                -> abortWith "None"


maddDeclaration :: (Declared i o, ProofData i, ProofData o) => Declaration(
  '[ Argument 'Required (Strategy i o)
   , Argument 'Required (Strategy i o)]
   :-> Strategy i o)
maddDeclaration =
  declare
    "sum"
    ["This processor runs both strategies in parallel and returns the successful ones."]
    (strat "left" ["The left strategy."], strat "right" ["The right strategy."])
    madd

madd :: (ProofData i, ProofData o, Show p) => Strategy i o -> Strategy i o -> Strategy i p
madd st1 st2 = processor Sum{left=st1,right=st2} .>>> close

