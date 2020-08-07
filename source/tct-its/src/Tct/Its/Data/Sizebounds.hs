module Tct.Its.Data.Sizebounds
  (
  Sizebounds
  , UpdateFun
  , initialise
  , boundsOfVars

  , updateSizebounds
  , sizebound
  , sizebounds
  , updateSizeboundsConstr
  , bridge
  , combine
  , module Tct.Its.Data.Bounds
  ) where

import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM
import qualified Data.List as L (intersect, lookup, nub, (\\), elemIndex, (!!))


import qualified Tct.Core.Common.Pretty as PP

import           Tct.Common.Ring
import qualified Tct.Common.Polynomial as P

import           Tct.Its.Data.ResultVariableGraph (RVGraph)
import qualified Tct.Its.Data.ResultVariableGraph as RVG
import           Tct.Its.Data.TransitionGraph (TGraph)
import qualified Tct.Its.Data.TransitionGraph as TG
import           Tct.Its.Data.Complexity as C
import           Tct.Its.Data.Bounds
import           Tct.Its.Data.Timebounds (Timebounds)
import qualified Tct.Its.Data.Timebounds as TB
import           Tct.Its.Data.LocalSizebounds (LocalSizebounds)
import qualified Tct.Its.Data.LocalSizebounds as LB
import           Tct.Its.Data.Rule (toGte)
import           Tct.Its.Data.Types
import Debug.Trace
--let trace _ x = x

type Sizebounds = Bounds RV

type UpdateFun = Rules -> TGraph -> RVGraph -> Timebounds -> Sizebounds -> LocalSizebounds -> Sizebounds

initialise :: LocalSizebounds -> Sizebounds
initialise lbounds = foldl (\acc k -> M.insert k Unknown acc) M.empty (M.keys lbounds)

updateSizebounds :: UpdateFun
updateSizebounds _ tgraph rvgraph tbounds sbounds lbounds = foldl k sbounds (RVG.sccs rvgraph)
  where
    k nsbounds scc = case scc of
      RVG.Trivial rv    -> sizebound tgraph lbounds rv nsbounds
      RVG.NonTrivial rvs -> sizebounds tbounds nsbounds lbounds rvgraph rvs



boundsOfVars :: Sizebounds -> (Int,Int) -> M.Map Var Complexity
boundsOfVars sbounds (t1,i1) = M.fromList [ (v,c) | (RV  t i v ,c) <- M.assocs sbounds , t == t1, i == i1 ]

-- sizebound for trivial SCCs
sizebound :: TGraph -> LocalSizebounds -> RV -> Sizebounds -> Sizebounds
sizebound tgraph lbounds rv sbounds = update rv sbound sbounds
  where
    lbound = lbounds `LB.lboundOf` rv
    sbound
      | null preds = lbound
      | otherwise  = foldl1 maximal [ compose lbound (varmap t' i)| (t',i) <- preds ]

    varmap t' i = M.fromList [ (v1,c1) | (RV t1 i1 v1, c1) <- M.assocs sbounds , t1 == t', i1 == i ]
    preds       = TG.predecessors tgraph (rvRule rv) 

cyclicDependencies :: RVGraph -> [RV] -> RV -> [Var]
cyclicDependencies rvgraph scc rv = L.nub [ rvVar rv' | rv' <- RVG.predecessors rvgraph rv `L.intersect` scc ]

dependencyConstraint :: RVGraph -> [RV] -> RV -> Bool
dependencyConstraint rvgraph rvs rv = length (cyclicDependencies rvgraph rvs rv) <= 1

sizebounds :: Timebounds -> Sizebounds -> LocalSizebounds -> RVGraph -> [RV] -> Sizebounds
sizebounds tbounds sbounds lbounds rvgraph scc 
  | not (null unbounds)                                         = sbounds
  | not (all (dependencyConstraint rvgraph scc . fst) sumpluss) = sbounds
  | otherwise = foldl (\sbounds' rv -> update rv cost sbounds') sbounds scc
  where 
    (maxs,maxpluss,sumpluss,unbounds) = classify lbounds scc
    cost = sizeboundOf tbounds sbounds lbounds rvgraph scc maxs maxpluss sumpluss


-- sepereate in classes
classify :: LocalSizebounds -> [RV] -> ([(RV, Int)],[(RV, Int)],[(RV, Int)],[RV])
classify lbounds = foldl k ([],[],[],[])
  where
    k (ms, mps, sps, us) rv = case lbounds `LB.lgrowthOf` rv of
      Max i     -> ((rv,i):ms , mps        , sps        , us)
      MaxPlus i -> (ms        , (rv,i):mps , sps        , us)
      SumPlus i -> (ms        , mps        , (rv,i):sps , us)
      Unbounded -> (ms        , mps        , sps        , rv:us)
      


sizeboundOf :: Timebounds -> Sizebounds -> LocalSizebounds -> RVGraph -> [RV] -> [(RV,Int)] -> [(RV,Int)] -> [(RV,Int)] -> Complexity
sizeboundOf tbounds sbounds lbounds rvgraph scc ms mps sps=
  sizeboundMax sbounds rvgraph scc ms
  `add` sizeboundMaxPlus tbounds mps
  `add` sizeboundSumPlus tbounds sbounds lbounds rvgraph scc sps

sizeboundsIncoming :: Sizebounds -> RVGraph -> [RV] -> [Complexity]
sizeboundsIncoming sbounds rvgraph = map (sbounds `boundOf`) . RVG.incoming rvgraph

sizeboundMax :: Sizebounds -> RVGraph -> [RV] -> [(RV,Int)] -> Complexity
sizeboundMax sbounds rvgraph scc ms = foldl1 maximal (constBound: incomBounds)
  where 
    constBound = poly . P.constant . maximum . (0:) . snd $ unzip ms
    incomBounds = sizeboundsIncoming sbounds rvgraph scc

-- | 
sizeboundMaxPlus :: Timebounds -> [(RV, Int)] -> Complexity
sizeboundMaxPlus tbounds mps = bigAdd $ map k mps
  where k (rv,i) = (tbounds `TB.tboundOf` rvRule rv) `mul` poly (P.constant i)


sizeboundSumPlus :: Timebounds -> Sizebounds -> LocalSizebounds -> RVGraph -> [RV] -> [(RV, Int)] -> Complexity
sizeboundSumPlus tbounds sbounds lbounds rvgraph scc sps = bigAdd $ map k sps
  where 
    k (rv,i) = (tbounds `TB.tboundOf` rvRule rv) `mul` ( poly (P.constant i) `add` bigAdd (map (f rv) (vars rv)))
    vars rv = activeVariables (lbounds `LB.lboundOf` rv) L.\\ cyclicDependencies rvgraph scc rv
    f rv v = foldl maximal zero [ sbounds `boundOf` rv' | rv' <- RVG.predecessors rvgraph rv, v == rvVar rv' ]


bridge :: Sizebounds -> RuleId -> [(RuleId,RuleId)] -> Sizebounds
bridge sb r rs =
  let
    sb1 = M.foldrWithKey (\rv _ m ->  if rvRule rv == r then M.delete rv m else m) sb sb
  in
  M.foldrWithKey (\rv c m -> 
    case lookup (rvRule rv) rs of
      Just r -> M.insert (rv{rvRule = r}) c m
      _ ->   m) sb1 sb1


combine :: Sizebounds -> RuleId -> RuleId-> RuleId -> Sizebounds
combine sb r r' rnew =
  let
    removed rv = rvRule rv `elem` [r,r']
    c' rv = case M.lookup (rv{rvRule = r'}) sb of {Just c -> c; _ -> Unknown }
    insert_max rv c m = M.insert rv{rvRule = rnew} (maximal c (c' rv)) m
    ins_update rv c m = if rvRule rv == r then insert_max rv c m else m-- trace (" rv:" ++ show rv) (
    ins sb = M.foldrWithKey ins_update sb sb
    del_update rv _ m = if removed rv then M.delete rv m else m
  in
  M.foldrWithKey del_update (ins sb) sb

updateSizeboundsConstr :: UpdateFun
updateSizeboundsConstr irules tgraph rvgraph tbounds sbounds lbounds =
  let sboundsn = foldl k sbounds (RVG.sccs rvgraph) in
  if sboundsn /= sbounds then updateSizeboundsConstr irules tgraph rvgraph tbounds sboundsn lbounds else sboundsn
  where
    k nsbounds scc = case scc of
      RVG.Trivial rv    -> sizebound tgraph lbounds rv nsbounds
      RVG.NonTrivial rvs -> sizeboundsConstr irules tbounds nsbounds lbounds rvgraph rvs

sizeboundsConstr :: Rules -> Timebounds -> Sizebounds -> LocalSizebounds -> RVGraph -> [RV] -> Sizebounds
sizeboundsConstr irules tbounds sbounds lbounds rvgraph scc 
  | not (null unbounds)                                         = sbounds
  | not (all (dependencyConstraint rvgraph scc . fst) sumpluss) = sbounds
  | otherwise = foldl (\sbounds' rv -> check_update rv sbounds') sbounds scc
  where
    (_,_,sumpluss,unbounds) = classify lbounds scc
    check_update rv sbs = if boundOf sbs rv == Unknown then check_constraint rv sbs else sbs
    check_constraint rv sbs = 
      let
        rl = irules IM.! (rvRule rv)
        v = rvVar rv
        r = head (rhs rl) -- consider only the case for one term on rhs
        idx = L.elemIndex (P.variable v) (args (lhs rl))
        rhs_v = (args r) L.!! (case idx of Just n -> n; _ ->  0)
        cs = con rl --- make sure cs is conjunction
        bnd_ps = [ (l, r) | cj <- cs, Gte l r <- cj ] ++ [ (l, r) | cj <- cs, Eq l r <- cj ]
        bnds = [ (l, - P.constantValue r) | (l, r) <- bnd_ps, v `elem` P.variables r, all (\c -> c >= 0) (P.coefficients r)]
        pre = RVG.predecessors rvgraph rv
        all_pre_bounds = [ boundsOfVars sbs (rvRule p, rvRpos p) | p <- pre ]
        pre_bounds = foldl (M.unionWith C.maximal) (head all_pre_bounds) (tail all_pre_bounds)

        bnd_exprs = [ P.substituteVariables rhs_v (M.fromList [(v, add p (P.constant rconst))]) | (p, rconst) <- bnds ]
        bnd_exprsx = [ if P.constantValue (p :: P.Polynomial Int Var) <= 0 then fst (P.splitConstantValue p) else p | p <- bnd_exprs]
        complex_bnds = [ compose (C.poly p) pre_bounds | p <- bnd_exprsx ]
        useful_bnds = [ p | p <- complex_bnds, not (C.Unknown == p) ]
      in
      if pre  == [] || length (rhs rl) > 1 || idx == Nothing || useful_bnds == [] then sbs
      else (trace ("ossible bnds " ++ show bnds ++ " cbns " ++ show complex_bnds) (M.insert rv (head useful_bnds) sbs))

ppSizebounds :: Vars -> Sizebounds -> PP.Doc
ppSizebounds vars sbounds = ppRVs vars (M.assocs sbounds) (\sbound -> [PP.pretty sbound])

instance {-# OVERLAPPING #-} PP.Pretty (Vars,Sizebounds) where
  pretty = uncurry ppSizebounds

