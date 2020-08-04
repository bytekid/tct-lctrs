module Tct.Its.Data.Complexity
  (
  Complexity (..)

  , unknown
  , poly
  , logbound
  , toComplexity
  , isLog
  , isPoly
  , isZero
  , isConstantIn
  , isLinearIn

  , minimal
  , maximal
  , compose
  , cadd

  , Growth (..)
  , growth
  , isSumPlus
  , activeVariables
  ) where


import qualified Data.Map.Strict as M

import qualified Tct.Core.Common.Pretty    as PP
import qualified Tct.Core.Data             as T

import qualified Tct.Common.Polynomial    as P
import           Tct.Common.Ring

import Tct.Its.Data.Types (IPoly, Var)
import Debug.Trace

data Complexity
  = Unknown
  | NPoly IPoly
  | LogPoly IPoly Int IPoly IPoly -- LogPoly(x,d,p,q) is log(x)^d*p + q
  deriving (Eq, Show)

data Growth 
  = Max Int      -- ^ > x' = x; x' = y; x' = 3
  | MaxPlus Int  -- ^ > x' = x + 1; x' = y + 3
  | SumPlus Int  -- ^ > x' = y + z; but not x' = x + z
  | Unbounded deriving (Eq,Ord,Show)

ppComplexity :: Complexity -> PP.Doc
ppComplexity Unknown   = PP.char '?'
ppComplexity (NPoly p) = PP.pretty p
ppComplexity (LogPoly x d p q) =
  PP.hcat [PP.text "log(", PP.pretty x, PP.text ")^", PP.int d, PP.text "*(",
    PP.pretty p, PP.pretty ") + ", PP.pretty q]

instance PP.Pretty Complexity where
  pretty = ppComplexity


unknown :: Complexity
unknown = Unknown

poly :: IPoly -> Complexity
poly = NPoly . P.mapCoefficients abs

logbound :: IPoly -> Int -> IPoly -> IPoly -> Complexity
logbound x d p q = LogPoly x d (cabs p) (cabs q) 
  where cabs = P.mapCoefficients abs

isPoly:: Complexity -> Bool
isPoly (NPoly _) = True
isPoly _ = False

isLog:: Complexity -> Bool
isLog (LogPoly x d p q) = p == P.fromView [(1, [])] && q == (P.constant 0)
isLog _ = False

isConstantIn:: Complexity -> Var -> Bool
isConstantIn compl y =
  case compl of
    (LogPoly x d p q) -> (d == 0 && constp (proj_y p) && constp (proj_y q))
    (NPoly p) -> constp (proj_y p)
  where 
    constp p = P.degree p <= 0 -- zero polynomial has degree -1
    proj_y p = P.fromView (filter (\m -> y `elem` (map fst) (snd m)) (P.toView p))

isLinearIn:: Complexity -> Var -> Bool
isLinearIn compl y = 
    case compl of 
      (LogPoly x d p q) ->
        ((lin (proj_y p) && d == 0) || P.degree (proj_y p) == 0) && lin (proj_y q)
      (NPoly p) -> lin (proj_y p)
  where 
    lin p = P.degree p <= 1
    proj_y p = P.fromView (filter (\m -> y `elem` (map fst) (snd m)) (P.toView p))

isZero :: Complexity -> Bool
isZero compl = 
  case compl of 
    (LogPoly x d p q) -> P.degree p < 0 && P.degree q < 0
    (NPoly p) -> P.degree p < 0


compareComplexity :: Complexity -> Complexity -> Maybe Ordering
compareComplexity Unknown Unknown   = Just EQ
compareComplexity Unknown (NPoly _) = Just GT
compareComplexity (NPoly _) Unknown = Just LT
compareComplexity l@(LogPoly _ _ _ _) (NPoly p) =
  compareComplexity l (LogPoly (P.constant 0) 0 (P.constant 0) p)
compareComplexity (NPoly p) (LogPoly _ d q r)
  | P.degree p == 0 && P.degree q + P.degree r > 0 = Just LT
compareComplexity (LogPoly _ d q r) (NPoly p)
  | P.degree p == 0 && P.degree q + P.degree r > 0 = Just GT
compareComplexity (NPoly p) (LogPoly _ d q r)
  | P.degree p == 0 && P.degree q + P.degree r > 0 = Just LT
compareComplexity (LogPoly x d q r) np@(NPoly p)
  | P.degree q == 0 && P.degree r < P.degree p && var_subset x p = Just LT
  where var_subset p q = all (\x -> x `elem` P.variables q) (P.variables p)
compareComplexity np@(NPoly p) (LogPoly x d q r)
  | P.degree q == 0 && P.degree r < P.degree p && var_subset x p = Just GT
  where var_subset p q = all (\x -> x `elem` P.variables q) (P.variables p)
compareComplexity (NPoly p) l@(LogPoly _ _ _ _) =
  compareComplexity (LogPoly (P.constant 0) 0 (P.constant 0) p) l
compareComplexity (LogPoly _ _ _ _) Unknown = Just LT
compareComplexity Unknown (LogPoly _ _ _ _) = Just GT
compareComplexity (NPoly p1) (NPoly p2)
  | all (==0) cs = Just EQ
  | all (>=0) cs = Just GT
  | all (<=0) cs = Just LT
  | otherwise    = Nothing
  where cs = P.coefficients (p1 `sub` p2)
compareComplexity (LogPoly x d p1 q1) (LogPoly x' d' p2 q2) -- FIXME: add cases?
  | p1 == P.constant 0 && p2 == P.constant 0 =
    compareComplexity (NPoly q1) (NPoly q2)
  | x == x' && q1 == P.constant 0 && q2 == P.constant 0 = 
    compareComplexity (NPoly p1) (NPoly p2)
  | x == x' && dp1 == dp2 && (dq1 == dq2 || (dq1 <= dp1 && dq2 <= dp2)) =
    if d < d' then Just LT else if d > d' then Just GT else Just EQ -- well, asymptotically
  | otherwise               = Nothing
  where
    cs = P.coefficients (p1 `sub` p2)
    ds = P.coefficients (q1 `sub` q2)
    dp1 = P.degree p1
    dp2 = P.degree p2
    dq1 = P.degree q1
    dq2 = P.degree q2

toComplexity :: Complexity -> T.Complexity
toComplexity Unknown = T.Unknown
toComplexity (NPoly p)
  | deg < 0   = T.Poly Nothing
  | otherwise = T.Poly (Just deg)
  where deg = P.degree p
toComplexity (LogPoly x d p q)
  | degp < 0 = T.LogPoly Nothing
  | otherwise = T.LogPoly (Just (d, max degp degq))
  where
    degp = P.degree p
    degq = P.degree q

minimal :: Complexity -> Complexity -> Complexity
minimal Unknown c2 = c2
minimal c1 Unknown = c1
minimal c1 c2      = case compareComplexity c1 c2 of {Just GT -> c2; _ -> c1}

maximal :: Complexity -> Complexity -> Complexity
maximal c1 c2 = case compareComplexity c1 c2 of
  Just EQ -> c1
  Just GT -> c1
  Just LT -> c2
  Nothing -> case (c1,c2) of
    (NPoly p1, NPoly p2) -> NPoly $ P.zipCoefficientsWith max p1 p2
    _                    -> c1 `cadd` c2

cadd :: Complexity -> Complexity -> Complexity
cadd Unknown _             = Unknown
cadd _ Unknown             = Unknown
cadd (NPoly p1) (NPoly p2) = NPoly (p1 `add` p2)
cadd (LogPoly x d p q) (LogPoly x' d' p' q') | x == x' =
  LogPoly x (max d d') (p `add` p') (q `add` q') -- FIXME overapproximation
cadd (LogPoly x d p q) (LogPoly x' d' p' q') =
  LogPoly (x `add` x') (d + d') (p `add` p') (q `add` q') -- FIXME overapproximation
cadd (LogPoly x d p q) (NPoly p') = LogPoly x (d) p (q `add` p')
cadd (NPoly p') (LogPoly x d p q) = LogPoly x (d) p (q `add` p')

cmul :: Complexity -> Complexity -> Complexity
cmul Unknown _             = Unknown
cmul _ Unknown             = Unknown
cmul (NPoly p1) (NPoly p2) = NPoly (p1 `mul` p2)
cmul (LogPoly x d p q) (LogPoly x' d' p' q') | x == x' =
  LogPoly x (d + d') (p `mul` p') (q `mul` q') -- FIXME overapproximation
cmul (LogPoly x d p q) (LogPoly x' d' p' q') =
  LogPoly (x `add` x') (d + d') (p `mul` p') (q `mul` q') -- FIXME overapproximation
cmul (LogPoly x d p q) (NPoly p') =
  LogPoly x (d) (p `mul` p') (q `mul` p')
cmul (NPoly p') (LogPoly x d p q) =
  LogPoly x (d) (p `mul` p') (q `mul` p')
  

instance Additive Complexity where
  zero = NPoly (P.constant 0)
  add  = cadd

instance Multiplicative Complexity where
  one = NPoly (P.constant 1)
  mul = cmul

activeVariables :: Complexity -> [Var]
activeVariables Unknown   = []
activeVariables (NPoly p) = P.variables p
activeVariables (LogPoly x _ p q) = P.variables x ++ P.variables p ++ P.variables q

compose :: Complexity -> M.Map Var Complexity -> Complexity
compose Unknown _ = Unknown
compose c@(NPoly p) m 
  | P.degree p <= 1 = c -- constant
  | all (`elem` dpolys) (activeVariables c) = poly $ subst p
  | is_var p && M.member (var p) m = m M.! (var p)
  | otherwise                               = trace ("compose NPoly undefined " ++ show p ++ " vs " ++ show m) Unknown
  where
    polys   = [ (v,np) | (v, NPoly np) <- M.toAscList m ]
    dpolys = fst (unzip polys)
    is_var p = case P.toView p of {[(c, [(x, d)])] | c == 1 && d == 1 -> True; _ -> False} 
    var p = fst (head (snd (head (P.toView p))))
    subst = \p -> P.substituteVariables p (M.fromAscList polys)
compose c@(LogPoly x d p q) m 
  | all (`elem` dpolys) (activeVariables c) = logbound (subst x) d (subst p) (subst q)
  | otherwise                               = trace "compose LogPoly undefined" Unknown
  where
    polys   = [ (v,np) | (v, NPoly np) <- M.toAscList m ]
    lpolys   = [ (v,p) | (v, LogPoly x d p q) <- M.toAscList m ]
    dpolys = fst (unzip polys)
    defined = dpolys ++ fst (unzip lpolys)
    subst_undef = Nothing `elem` (map (\v -> M.lookup v (M.fromAscList polys)) (activeVariables c))
    subst = \p -> P.substituteVariables p (M.fromAscList polys)


growth :: Complexity -> Growth
growth Unknown = Unbounded
growth (NPoly p)
  | not (P.isStronglyLinear p) = Unbounded
  | ncoeffs == 0               = Max c
  | ncoeffs == 1 && c == 0     = Max 0
  | ncoeffs == 1               = MaxPlus c
  | otherwise                  = SumPlus c
  where 
    (cs,c)  = P.coefficients' p
    ncoeffs = length cs

instance PP.Pretty Growth where
  pretty (Max i)     = PP.text ".=" PP.<+> PP.int i
  pretty (MaxPlus i) = PP.text ".+" PP.<+> PP.int i
  pretty (SumPlus i) = PP.text ".*" PP.<+> PP.int i
  pretty Unbounded   = PP.text ".?"

isSumPlus :: Growth -> Bool
isSumPlus (SumPlus _) = True
isSumPlus _           = False
