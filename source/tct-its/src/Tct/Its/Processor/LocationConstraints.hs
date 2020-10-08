{-# LANGUAGE ScopedTypeVariables #-}

module Tct.Its.Processor.LocationConstraints
  ( locationConstraints
  ) where

import           Control.Monad
import qualified Data.Map.Strict              as M
import qualified Data.IntMap.Strict           as IM
import           Data.Maybe                         (fromMaybe)
import qualified Data.List                    as L  (nub)
import qualified Data.Set                     as S  (fromList, isSubsetOf) 
import qualified Data.Graph.Inductive         as Gr (lpre)
import           Tct.Its.Data.TransitionGraph       (nonTrivialSCCs)

import qualified Tct.Core.Common.Pretty       as PP
import qualified Tct.Core.Common.Xml          as Xml
import qualified Tct.Core.Data                as T

import           Tct.Common.ProofCombinators

import           Tct.Its.Data.Types
import           Tct.Its.Data.Problem
import           Tct.Its.Data.Rule
import qualified Tct.Common.Polynomial        as P
import qualified Tct.Common.SMT as SMT
-- import           Debug.Trace
trace _ x = x

data LocationConstraintProcessor = LocationConstraintsProc deriving Show

data LocationConstraintsProof =
  LocationConstraintsProof LocationConstraints
  | LocationConstraintsFail
  deriving Show

ppLocationConstraints :: LocationConstraints -> PP.Doc
ppLocationConstraints lcs = PP.hsep [ PP.hsep [PP.pretty r, PP.text ": ", PP.pretty c] | (r, c) <- M.assocs lcs]

instance PP.Pretty LocationConstraintsProof where
  pretty (LocationConstraintsProof lcs) = PP.hsep
    [ PP.text "We computed the location constraints "
    , ppLocationConstraints lcs
    , PP.dot ]
  pretty LocationConstraintsFail = PP.text "LocationConstraints: no progress."

instance Xml.Xml LocationConstraintsProof where
  toXml (LocationConstraintsProof _ ) = Xml.elt "location constraints" []
  toXml _                        = Xml.elt "no location constraints" []

instance T.Processor LocationConstraintProcessor where
  type ProofObject LocationConstraintProcessor = ApplicationProof LocationConstraintsProof
  type In  LocationConstraintProcessor         = Its
  type Out LocationConstraintProcessor         = Its
  type Forking LocationConstraintProcessor     = T.Optional T.Id

  execute LocationConstraintsProc prob | isClosed prob = closedProof prob
  execute LocationConstraintsProc prob = do
    nprob <- setLocationConstraints prob
    nnprob <- findInvariants nprob
    let pproof = LocationConstraintsProof (error "proc locconstr" `fromMaybe` locConstraints_ nnprob)
    if locConstraints_ prob /= locConstraints_ (trace "after locconstr" nnprob)
      then progress (Progress nnprob) (Applicable pproof)
      else progress NoProgress (Applicable LocationConstraintsFail)

locationConstraints :: ItsStrategy
locationConstraints = T.Apply LocationConstraintsProc

setLocationConstraints :: Its -> T.TctM Its
setLocationConstraints prob = 
  updateLocationConstraints prob >>= \probnew ->  
  if locConstraints_ prob == locConstraints_ probnew then return prob else updateLocationConstraints probnew

updateLocationConstraints :: Its -> T.TctM Its
updateLocationConstraints prob = do
  lcs <- foldM (\lcs r -> compute_lcs r >>= \cc -> return (M.insert r cc lcs)) M.empty (rulesIds (irules_ prob))
  return (prob { locConstraints_ = Just lcs })
  where
    compute_lcs rid2 = let
        rl2 = irules_ prob IM.! rid2
        pres = [ (src,tgt) | (src,tgt) <- Gr.lpre (tgraph_ prob) rid2, src /= rid2 ] -- predecessors
        has_single_rhs rid1 = length (rhs (irules_ prob IM.! rid1)) == 1
        has_conj_constr rid1 = length (con (irules_ prob IM.! rid1)) > 0
        do_consider rid1 = has_single_rhs rid1 && has_conj_constr rid1
        lcs0 = case locConstraints_ prob of {Just lcs -> lcs; _ -> M.empty} 
        prop_constr rid1 = let
            rl1 = irules_ prob IM.! rid1
            argpairs = zip (args (head (rhs rl1))) (args (lhs rl2))
            unchanged = S.fromList (concat [ P.variables r | (r,l) <- argpairs, r == l ])
            polys c = case c of  {Eq p1 p2 -> [p1, p2]; Gte p1 p2 -> [p1, p2]}
            vars c = S.fromList (concatMap P.variables (polys c))
          in
          [[ c | c <- head (con rl1), vars c `S.isSubsetOf` unchanged ]]
        implies rid constr = let
            rl = irules_ prob IM.! rid
            lconstr = case lcs0 M.!? rid of {Just c -> c; _ -> []} 
            rule_constr = SMT.bigAnd (map (SMT.bigOr . (map encodeAtom)) (trace ("check whether " ++ show (lconstr ++ (con rl)) ++ " implies " ++ show constr) (lconstr ++ (con rl))))
            other_constr = SMT.bigAnd (map (SMT.bigOr . (map encodeAtom)) constr)
          in
          impliesSMT rule_constr other_constr
      in
      case (trace ("rule " ++ show rid2 ++ " has predecessors: " ++ show pres) pres) of -- only for single predecessor
        [(p, _)] | do_consider p -> return (prop_constr p)
        [(p, _), (q, _)] | do_consider p && do_consider q -> do
          let
            pc = prop_constr p
            qc = prop_constr q
          b1 <- if qc == [[]] then return False else implies p qc
          b2 <- if pc == [[]] then return False else implies q pc
          if trace ("implies "++ show b1 ++ ", " ++ show b2) b1 then return qc else if b2 then return pc else return []
        _ -> return []

impliesSMT :: SMT.Formula Var -> SMT.Formula Var -> T.TctM Bool
impliesSMT f g = do
  s :: SMT.Result () <- SMT.smtSolveSt SMT.yices $ do
    SMT.setLogic SMT.QF_NIA
    SMT.assert (f SMT..&& SMT.bnot g)
    return $ SMT.decode ()
  return $ not $ SMT.isSat s

implies :: Constraint -> Constraint -> Bool
implies f g = all (`elem` f) g

checkInvariant :: Its -> [RuleId] -> Atom -> T.TctM Bool
checkInvariant prob rids inv = foldM (\b rl -> if not b then return False else checkForRule rl) True rids
  where
    checkForRule rid = let
        rl = irules_ prob IM.! rid
        has_single_rhs = length (rhs (irules_ prob IM.! rid)) == 1
        update = M.fromList (zip (map tovar (args (lhs rl))) (args (head (rhs rl))))
        assumption = [inv] : (lcs_lookup rid) ++ (con rl)
        consequence = [[subst update inv]]
      in
      if not has_single_rhs then return False
      else if implies assumption consequence then return True
      else impliesSMT (toSMT assumption) (toSMT consequence) >>= \b -> return (trace (if b then (" " ++ show rid ++ " ok") else (" " ++ show rid ++ " fails " ++ show inv)) b)
    lcs = case locConstraints_ prob of {Just lcsm -> lcsm; _ -> M.empty}
    lcs_lookup rid = case lcs M.!? rid of {Just c -> c; _ -> []}
    subst s (Gte p q) = Gte (P.substituteVariables p s) (P.substituteVariables q s)
    subst s (Eq p q) = Eq (P.substituteVariables p s) (P.substituteVariables q s)
    tovar p = case P.variables p of {[x] -> x; xs -> head (trace "non-var lhs" xs)}
    toSMT c = SMT.bigAnd (map (SMT.bigOr . (map encodeAtom)) c)

findInvariantsSCC :: Its -> [RuleId] -> T.TctM Its
findInvariantsSCC prob scc = do
  invs <- filterM valid invCandidates
  return (prob { locConstraints_ = Just (foldl add lcs0 invs) })
  where
    lcs0 = case locConstraints_ prob of {Just lcsm -> lcsm; _ -> M.empty}
    allrules = IM.elems (irules_ prob)
    invCandidates = L.nub (concat $ concat (map con allrules))
    pres = [ src | rid <- scc, (src,_) <- Gr.lpre (tgraph_ prob) rid, not (rid `elem` scc) ]
    valid inv = checkInvariant prob (pres ++ scc) (trace ("checking for SCC " ++ show scc ++ " " ++ show inv) inv)
    loc_constr rid = case lcs0 M.!? rid of {Just c -> c; _ -> []}
    add lcs inv = foldl (\lcsm rid -> M.insert rid ([inv] : (loc_constr rid)) lcsm) lcs scc 

findInvariants :: Its -> T.TctM Its
findInvariants prob =
  foldM findInvariantsSCC prob (nonTrivialSCCs (tgraph_ prob))