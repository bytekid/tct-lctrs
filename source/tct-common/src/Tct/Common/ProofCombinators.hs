-- | Some handy combinators for proofs.
{-# LANGUAGE DeriveFunctor #-}
module Tct.Common.ProofCombinators 
 ( 
 OrientationProof (..)
 , ApplicationProof (..)
 , ApplicationProofT (..)
 ) where


import Control.Monad.Trans
import Control.Monad
import qualified Tct.Core.Common.Pretty as PP
import qualified Tct.Core.Common.Xml    as Xml


data OrientationProof o
  = Order o
  | Incompatible
  deriving (Show, Functor)

instance PP.Pretty o => PP.Pretty (OrientationProof o) where
  pretty (Order o)        = PP.pretty o
  pretty Incompatible     = PP.paragraph "The input can not be schown compatible."

instance Xml.Xml o => Xml.Xml (OrientationProof o) where
  toXml (Order o)        = Xml.toXml o
  toXml Incompatible     = Xml.elt "incompatible" []

  toCeTA (Order o)        = Xml.toCeTA o
  toCeTA Incompatible     = Xml.unsupported

-- | A proof combinator that provides a cut evaluation.
data ApplicationProof p
  = Inapplicable String
  | Closed
  | Applicable p
  deriving (Show, Functor)

newtype ApplicationProofT m p = ApplicationT { runApplicationT :: m (ApplicationProof p) }

-- | Either like data-type where we fix the Left/Error type to String.
instance Applicative ApplicationProof where
  pure                   = Applicable
  Inapplicable msg <*> _ = Inapplicable msg
  Closed  <*> _          = Closed
  Applicable f <*> a     = fmap f a

instance PP.Pretty p => PP.Pretty (ApplicationProof p) where
  pretty (Inapplicable s) = PP.paragraph $ "The processor is not applicable. The reason is " ++ s ++ "."
  pretty Closed           = PP.text "The problem is already solved."
  pretty (Applicable p)   = PP.pretty p

instance Xml.Xml p => Xml.Xml (ApplicationProof p) where
  toXml (Inapplicable s) = Xml.elt "inapplicable" [Xml.text s]
  toXml Closed           = Xml.elt "closed" []
  toXml (Applicable p)   = Xml.toXml p

  toCeTA Closed           = Xml.elt "rIsEmpty" []
  toCeTA (Applicable p)   = Xml.toCeTA p
  toCeTA (Inapplicable _) = Xml.unsupported


instance Monad ApplicationProof where
  return                 = Applicable
  Inapplicable msg >>= _ = Inapplicable msg
  Closed >>= _           = Closed
  Applicable p >>= k     = k p

instance Functor f => Functor (ApplicationProofT f) where
  fmap f = ApplicationT . fmap (fmap f) . runApplicationT

instance (Functor a, Monad a) => Applicative (ApplicationProofT a) where
  pure  = return
  (<*>) = ap

instance Monad m => Monad (ApplicationProofT m) where
  return = ApplicationT . return . return
  m >>= k = ApplicationT $ do
    a <- runApplicationT m
    case a of
      Inapplicable msg -> return (Inapplicable msg)
      Closed           -> return Closed
      Applicable p     -> runApplicationT (k p)

instance MonadTrans ApplicationProofT where
  lift = ApplicationT . liftM Applicable

instance MonadIO m => MonadIO (ApplicationProofT m) where
  liftIO = lift . liftIO

