-- | This module provides the 'ProofTree' type.
-- 'ProofTree' stores the open problems and the proofs of applying 'Processor' instances to problems.
module Tct.Core.Data.ProofTree
  (
  -- * ProofTree
  ProofNode (..)
  , ProofTree (..)
  , open
  , size
  , flatten
  , substitute
  , substituteM
  -- * Certification
  , certificate
  , certificateWith
  -- * Properites
  , isOpen
  , isClosed
  , isFailing
  , isProgressing
  -- * Output
  , ppProofTree
  , ppProofTreeLeafs
  ) where


import qualified Data.Foldable             as F (toList)

import qualified Tct.Core.Common.Pretty    as PP
import           Tct.Core.Data.Answer      (termcomp)
import           Tct.Core.Data.Certificate (Certificate, unbounded)
import           Tct.Core.Data.Types

-- | Returns the 'Open' nodes of a 'ProofTree'.
open :: ProofTree l -> [l]
open = foldr (:) []

-- | Returns the number of nodes of a 'ProofTree'.
size :: ProofTree l -> Int
size (Open _)          = 1
size (Failure _)       = 1
size (Success _ _ pts) = 1 + sum (size <$> pts)

-- | Monadic version of 'substitute'.
substituteM :: (Functor m, Monad m) => (l -> m (ProofTree k)) -> ProofTree l -> m (ProofTree k)
substituteM s (Open l)            = s l
substituteM _ (Failure r)         = return (Failure r)
substituteM s (Success pn cf pts) = Success pn cf <$> mapM (substituteM s) pts

-- | Substitute the open leaves of a proof tree according to the given function
substitute :: (l -> ProofTree k) -> ProofTree l -> ProofTree k
substitute f  (Open l)           = f l
substitute _ (Failure r)         = Failure r
substitute f (Success pn cf pts) = Success pn cf (substitute f `fmap` pts)

-- | Flattens a nested prooftree.
flatten :: ProofTree (ProofTree l) -> ProofTree l
flatten = substitute id

-- | Computes the 'Certificate' of 'ProofTree'.
collectCertificate :: ProofTree Certificate -> Certificate
collectCertificate (Open c)                     = c
collectCertificate Failure{}                    = unbounded
collectCertificate (Success _ certfn' subtrees) = certfn' (collectCertificate `fmap` subtrees)

-- | Computes the 'Certificate' of a 'ProofTree'.
-- 'Open' nodes have the 'Certificate' 'unboundend'.
--
-- prop> certificate pt = collectCertificate (const unbounded `fmap` pt)
certificate :: ProofTree l -> Certificate
certificate pt = collectCertificate $ const unbounded `fmap` pt

-- | Computes the 'Certificate' of a 'ProofTree'.
-- 'Open' nodes have the 'Certificate' provided certificate'.
--
-- prop> certificate pt cert = collectCertificate (const cert `fmap` pt)
certificateWith :: ProofTree l -> Certificate -> Certificate
certificateWith pt cert = collectCertificate $ const cert `fmap` pt


-- | Checks if the 'ProofTree' contains a 'Failure' node.
isFailing :: ProofTree l -> Bool
isFailing Failure{}         = True
isFailing (Success _ _ pts) = any isFailing pts
isFailing _                 = False

-- | Checks that the 'ProofTree' does not contain a 'Failure' node
-- and not consist of a single 'Open' node
isProgressing :: ProofTree l -> Bool
isProgressing (Open _) = False
isProgressing p        = not (isFailing p)

-- | Checks if there exists 'Open' nodes in the 'ProofTree'.
isOpen :: ProofTree l -> Bool
isOpen = not . isClosed

-- | Checks if there are no 'Open' nodes in the 'ProofTree'.
--
-- prop> isClosed = not . isOpen
isClosed :: ProofTree l -> Bool
isClosed = null . open


--- * Pretty Printing ------------------------------------------------------------------------------------------------

data Path = Path Int [(Int,Int)]

inc :: Path -> Path
inc (Path i is) = Path (succ i) is

split :: Path -> Int -> Path
split (Path i is) j = Path 1 ((i,j):is)

pathLength :: Path -> Int
pathLength (Path _ is) = 1 + length is

ppPath :: Path -> PP.Doc
ppPath (Path i is) = PP.cat $ PP.punctuate PP.colon . reverse $ PP.int i : f `fmap` is
  where
    f (j,k) = PP.int j PP.<> PP.dot PP.<> PP.text (g k)
    g n = if n >= 1 && n <= 26 then [toEnum (96+n)] else toEnum n : g (n-26)

ppNode :: (Show p, PP.Pretty prob, PP.Pretty po) => p -> prob -> po -> PP.Doc
ppNode p prob po = PP.vcat
  [ block "Considered Problem" (PP.pretty prob)
  , block "Applied Processor"  (PP.text $ show p)
  , block "Details"            (PP.pretty po) ]
  where block n e = PP.nest 4 (PP.text "+" PP.<+> PP.text n PP.<> PP.char ':' PP.<$$> e)

ppProofNode :: Processor p => ProofNode p -> PP.Doc
ppProofNode (ProofNode p prob po) = ppNode p prob po

ppReason :: Reason -> PP.Doc
ppReason (Failed proc prob reason) = ppNode proc prob reason
ppReason r                         = PP.pretty r

ppProofTree' :: Path -> (prob -> PP.Doc) -> ProofTree prob -> PP.Doc
ppProofTree' is ppProb pt@(Open l) = PP.vcat
  [ ppHeader pt is (PP.text "Open")
  , PP.indent 4 (ppProb l) ]
ppProofTree' is _ f@(Failure r) =
   ppHeader f is (PP.text "Failure")
   PP.<$$> PP.indent 2 (ppReason r)


ppProofTree' path ppProb pt@(Success pn _ pts) =
  PP.vcat [ ppHeader pt path (PP.text (takeWhile (`notElem` " {") (show (appliedProcessor pn))))
          , PP.indent 4 (ppProofNode pn)
          , ppSubTrees (F.toList pts) ]
  where
    ppSubTrees []  = PP.empty
    ppSubTrees [t] = ppProofTree' (inc path) ppProb t
    ppSubTrees ls  = PP.vcat [ ppProofTree' (split path j) ppProb t | (j,t) <- zip [1..] ls]


ppHeader :: ProofTree l -> Path -> PP.Doc -> PP.Doc
ppHeader pt p s =
  PP.text (replicate (pathLength p) '*') PP.<+> PP.text "Step" PP.<+> ppPath p PP.<> PP.char ':'
  PP.<+> s
  PP.<+> PP.group (PP.pretty (termcomp (certificate pt)))

ppProofTree :: (l -> PP.Doc) -> ProofTree l -> PP.Doc
ppProofTree pp pt =
  ppProofTree' (Path 1 []) pp pt
  PP.<> if null (F.toList pt) then PP.empty else
    PP.empty
    PP.<$$> PP.text "Following problems could not be solved:"
    PP.<$$> PP.indent 2 (ppProofTreeLeafs pp pt)

ppProofTreeLeafs :: (l -> PP.Doc) -> ProofTree l -> PP.Doc
ppProofTreeLeafs pp = PP.vcat . map pp . F.toList

instance PP.Pretty prob => PP.Pretty (ProofTree prob) where
  pretty = ppProofTree PP.pretty

