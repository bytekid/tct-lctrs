-- | This module provides a simple /Transform/ processor with a minimal proof output.
module Tct.Core.Processor.Transform
  ( transform
  , transform')
  where


import qualified Tct.Core.Common.Pretty as PP
import qualified Tct.Core.Common.Xml    as Xml
import           Tct.Core.Data


-- | @Transform descibe transform@
--
-- @describe@  A short description that is used in the proof output.
-- @trasnform@ A failing transformation.
data Transform i o = Transform String (i -> Either String o)

instance Show (Transform i o) where
  show _ = "Problem Transformation."

data TransformProof i o
  = TransformProof String i o
  | TransformFail String
  deriving Show

instance (ProofData i, ProofData o) => Processor (Transform i o) where
  type ProofObject (Transform i o) = TransformProof i o
  type In (Transform i o)          = i
  type Out (Transform i o)         = o

  execute (Transform msg t) prob = case t prob of
    Left err  -> abortWith err;
    Right new -> succeedWith1 (TransformProof msg prob new) fromId new

-- | The /Transform/ strategy.
transform :: (ProofData i, ProofData o)
  => String                 -- ^ Description of the transformation; used in the proof output.
  -> (i -> Either String o) -- ^ A (possible) failing transformation.
  -> Strategy i o
transform msg = processor . Transform msg

-- | >transform' = transform ""
transform' :: (ProofData i, ProofData o) => (i -> Either String o) -> Strategy i o
transform' = transform ""

instance (PP.Pretty i, PP.Pretty o) => PP.Pretty (TransformProof i o) where
  pretty (TransformFail err)      = PP.text $ "The transformation failed. The reason is " ++ err
  pretty (TransformProof msg i o) = PP.vcat
    [ (if null msg then PP.empty else PP.text msg) PP.<+> PP.text "The problem"
    , PP.indent 2 $ PP.pretty i
    , PP.text "is transformed into the problem"
    , PP.indent 2 $ PP.pretty o ] 

instance (Xml.Xml i, Xml.Xml o) => Xml.Xml (TransformProof i o) where
  toXml (TransformFail err)      = Xml.elt "transformation" [ Xml.elt "failed" [Xml.text err]]
  toXml (TransformProof msg i o) =
    Xml.elt "transformation"
      [ Xml.text msg
      , Xml.toXml i
      , Xml.toXml o ]

