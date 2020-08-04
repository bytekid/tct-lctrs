{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- local size bound
-- Sl(t,v')(m) >= sup { |v'(v)| exists l,v,l',v' . v <= m /\ (l,v) -> (l',v') for all |t,v'|
-- in other the lhs is a bound (as linear expression) on a rhs variable

-- should be computable as long we have linear bounds
-- has to be computed only once


module Tct.Its.Data.LocalSizebounds
  (
  LocalSizebounds
  , compute
  , computeWith
  , Minimize (..)
  -- * Queries
  , lboundOf
  , lgrowthOf
  ) where


import           Control.Monad         (liftM, when)
import           Data.Maybe (fromMaybe)
import qualified Data.IntMap.Strict       as IM
import qualified Data.Map.Strict       as M
import Data.List (intersect)
import qualified Data.Set as S
import qualified Data.List as L (partition)

import           Tct.Core.Data (TctM)
import qualified Tct.Core.Common.Pretty     as PP
import qualified Tct.Common.Polynomial as P
import           Tct.Common.Ring
import qualified Tct.Common.SMT as SMT
import           Tct.Common.SMT as SMT ((.&&), (.>=))

import           Tct.Its.Data.Complexity
import           Tct.Its.Data.Rule
import           Tct.Its.Data.Types
import           Debug.Trace


type LocalSizebounds = M.Map RV (Complexity, Growth)

type APoly  = P.Polynomial (SMT.IExpr String) Var
type IPolyV = P.PView Coefficient Var

data Coefficient
  = SomeCoefficient 
  | RestrictCoefficient 
  | IntCoefficient Int
  deriving (Eq,Ord,Show)

-- Add minimisation constraints
-- invokes optimathsat
data Minimize = Minimize | NoMinimize deriving (Eq, Ord, Show)

lboundOf :: LocalSizebounds -> RV -> Complexity
lboundOf lbounds rv = fst (error err `fromMaybe` M.lookup rv lbounds)
  where err = "Tct.Its.Data.LocalSizebounds.lboundOf: key '" ++ show rv ++ "' not defined."

lgrowthOf :: LocalSizebounds -> RV -> Growth
lgrowthOf lbounds rv = snd (error err `fromMaybe` M.lookup rv lbounds)
  where err = "Tct.Its.Data.LocalSizebounds.lgrowthOf: key '" ++ show rv ++ "' not defined."


compute :: Vars -> Rules -> TctM LocalSizebounds
compute = computeWith NoMinimize

computeWith :: Minimize -> Vars -> Rules -> TctM LocalSizebounds
computeWith minimize vs rs = M.unions `liftM` sequence (IM.foldrWithKey k [] rs)
  where k i r acc = computeRule minimize vs (i,r) : acc

computeRule :: Minimize -> Vars -> (RuleId, Rule) -> TctM LocalSizebounds
computeRule minimize vs ir = M.fromList `liftM` mapM k (rvss vs ir)
  where k (rv,rpoly,cpolys) = computeVar minimize vs rpoly cpolys >>= \c -> return (rv,c)

rvss :: Vars -> (RuleId, Rule) -> [(RV, IPoly, [APoly])]
rvss vs (ruleIdx, Rule l rs cs) =
  [ (RV ruleIdx rhsIdx v, rpoly, cs')
    | (rhsIdx, r) <- zip [0..] rs, (v, rpoly) <- zip vs (args r) ]
  -- SW do not use clauses with multiple disjuncts
  where cs' = [ P.mapCoefficients SMT.num p | [Gte p q] <- filterLinear (toGte cs) ]

computeVar :: Minimize -> Vars -> IPoly -> [APoly] -> TctM (Complexity, Growth)
computeVar minimize vs rpoly cpolys = fromMaybe (error "computeVar") `liftM` foldl1 liftMPlus
  [ 
  -- direct
  return $ if null cs then Just (poly rpoly, Max (abs c)) else Nothing -- constant
  , return $ if rpoly' `elem` pvars then Just (poly rpoly, Max 0) else Nothing -- variable
  -- indirect simple
  , solveRestricted []
  , solveRestricted dependentvs
  , solveSome       dependentvs
  , whenM (dependentvs /= vs) (solveRestricted vs)
  , whenM (dependentvs /= vs) (solveSome       vs)
  -- last resort
  , return  unbounded ]
  where 
    (cs,c)    = P.coefficients' rpoly
    pvars     = map P.variable vs
    rpoly'    = P.mapCoefficients abs rpoly
    liftMPlus m1 m2 = m1 >>= \m1' -> maybe m2 (return . Just) m1'
    whenM False _ = return Nothing
    whenM True d  = d

    dependentvs = vs `intersect` S.toList (deps rvs rvs (map P.variables cpolys)) where
      rvs = S.fromList (P.variables rpoly)
      deps new old vss
        | S.null new || null vss = old
        | otherwise              = deps (new' `S.difference` old) (old `S.union` new') vss2
        where 
          (vss1,vss2) = L.partition (any (`S.member` old)) vss
          new'        = S.unions $ map S.fromList vss1

    isConstant m    = P.mfromView m == one
    solveWith k ls  = entscheide minimize (P.linear k ls) rpoly cpolys
    solveRestricted = solveWith $ \m -> if isConstant m then SomeCoefficient else RestrictCoefficient
    solveSome       = solveWith $ const SomeCoefficient

    unbounded = Just (Unknown, Unbounded)


instance (SMT.Decode m c a, Additive a, Eq a)
  => SMT.Decode m (P.PView c Var) (P.Polynomial a Var) where
  decode = P.fromViewWithM SMT.decode

entscheide :: Minimize -> IPolyV -> IPoly -> [APoly] -> TctM (Maybe (Complexity, Growth))
entscheide minimize lview rpoly cpolys = do
  res <- entscheide' minimize lview rpoly cpolys
  return $ case res of
    SMT.Sat (lp,ap) -> let r = poly lp `maximal` poly ap in Just (r,growth r)
    _               -> Nothing


entscheide' :: Minimize -> IPolyV -> IPoly -> [APoly] -> TctM (SMT.Result (IPoly,IPoly))
entscheide' minimize lview rpoly cpolys = do
  res :: SMT.Result (IPoly,IPoly) 
    <- SMT.smtSolveSt (if minimize == Minimize then SMT.optimathsat else SMT.yices) $ do
    SMT.setLogic SMT.QF_LIA

    let
      interpretLhs = P.fromViewWith id
      interpretRhs = P.mapCoefficients SMT.num
      absolute p = SMT.bigAnd [ c SMT..== SMT.zero | c <- P.coefficients p ]

    let
      rinst = interpretRhs rpoly
      eliminate ply css = do
        let
          k p = SMT.nvarM' >>= \lambda -> return (lambda `P.scale` p)
        cs2 <- mapM k css
        let
          (p1,pc1) = P.splitConstantValue ply
          (p2,pc2) = P.splitConstantValue (bigAdd cs2)
        return $ absolute (p1 `sub` p2) SMT..&& (pc1 SMT..>= pc2)

      restrictVar (SomeCoefficient,m)     = SMT.ivarM' >>= \c' -> return (c',m)
      restrictVar (RestrictCoefficient,m) = SMT.sivarM' >>= \c' -> return (c',m)
      restrictVar (IntCoefficient i,m)    = return (SMT.num i, m)


    -- upper bound 
    uapoly <- mapM restrictVar lview
    let ubounded = (interpretLhs uapoly `sub` rinst) `eliminate` cpolys
    
    -- lower bound
    lapoly <- mapM restrictVar lview
    let lbounded = (rinst `sub` interpretLhs lapoly) `eliminate` cpolys

    SMT.assert =<< ubounded
    SMT.assert =<< lbounded

    when (minimize == Minimize) $ do
      tpoly <- mapM restrictVar lview
      SMT.assert $ SMT.bigAnd $ [ c .>= SMT.num 0 | (c,_) <- tpoly ]
      SMT.assert $ SMT.bigAnd $ [ t .>= u .&& t .>= neg u | ((t,_),(u,_)) <- zip tpoly uapoly ]
      SMT.assert $ SMT.bigAnd $ [ t .>= u .&& t .>= neg u | ((t,_),(u,_)) <- zip tpoly lapoly ]
      SMT.assert $ SMT.minimize $ SMT.bigAdd  [ c | (c,_) <- tpoly ]
   
    return $ SMT.decode (lapoly, uapoly)
  
  return res

ppLocalSizebounds :: Vars -> LocalSizebounds -> PP.Doc
ppLocalSizebounds vars lbounds = ppRVs vars (M.assocs lbounds) ppLbound
  where ppLbound (lbound, lgrowth)  = [PP.pretty lbound, PP.comma, PP.space, PP.pretty lgrowth]
  

instance {-# OVERLAPPING #-} PP.Pretty (Vars, LocalSizebounds) where
  pretty = uncurry ppLocalSizebounds

