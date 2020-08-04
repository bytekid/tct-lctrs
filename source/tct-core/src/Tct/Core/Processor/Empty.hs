-- | This module provides the /Empty Processor/.
-- If the given condition is fulfilled, the tree is closed with a constant certificate.
module Tct.Core.Processor.Empty (empty) where


import qualified Tct.Core.Common.Pretty   as PP
import           Tct.Core.Common.SemiRing (zero)
import qualified Tct.Core.Common.Xml      as Xml
import qualified Tct.Core.Data            as T


data Empty prob = Empty (prob -> Bool)

instance Show (Empty prob) where
  show _ = "EmptyProcessor"

data EmptyProof
  = EmptyProblem
  | OpenProblem
  deriving Show

instance PP.Pretty EmptyProof where
  pretty EmptyProblem = PP.text "The problem is already closed. The intended complexity is O(1)."
  pretty OpenProblem  = PP.text "The problem is still open."

instance Xml.Xml EmptyProof where
  toXml EmptyProblem  = Xml.elt "closed" []
  toXml OpenProblem   = Xml.elt "open" []

  toCeTA EmptyProblem = Xml.elt "rIsEmpty" []
  toCeTA _            = Xml.unsupported

instance T.ProofData prob => T.Processor (Empty prob) where
  type ProofObject (Empty prob) = EmptyProof
  type In  (Empty prob)         = prob
  type Out (Empty prob)         = prob
  type Forking (Empty prob)     = T.Judgement

  execute (Empty f) prob
    | f prob    = T.succeedWith0 EmptyProblem (T.judgement zero)
    | otherwise = T.abortWith OpenProblem

empty :: T.ProofData i => (i -> Bool) -> T.Strategy i i
empty = T.processor . Empty

