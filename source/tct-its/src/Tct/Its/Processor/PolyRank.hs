{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

{-

some explanation of the encoding does not hurt:

Linear ranking functions based on the farkas lemma [1,2]:

example:

forall X.
a1*x+b1*y+c1 >=0
a2*x+b2*y+c2 >=0
==> a'*x+b'y+c' >=0

exist la1, la2 >=0
a' = la1*a1+la2*a2 /\ b' = la1*b1+la2*b2 /\ c' >= la1*c1+la2+c2
Basically we just multiply each constraint with a fresh lambda symbol and sum them up.
Then we establish equality constraints between the coefficients of a variable and an inequality constraint for the constant part.
The farkas lemma states the equivalence of the forall an exist formula.


Polynomial ranking functions based on generalised farkas lemma [3]:
Sum_i p_i > 0 and Sum_j p_j >= 0 is unsat if there exists non-negative mu_0, mu_i, mu_j
where one of mu_0, mu_i is positive st
mu_0 + Sum_i mu_i*p_i + Sum_j mu_j*p_j = 0
decrease:
forall X . And p_i(X) >= 0 => l(X) - r(X) > 0
-> forall X . not (And p_i(X) >= 0) or l(X) - (r(x) + strict) >= 0
-> not(not (And p_i(X) >= 0) or l(X) - (r(x) + strict) >= 0) is unsat
-> (And p_i(X) >= 0) and not(l(X) - (r(x) + strict) >= 0)) is unsat
-> (And p_i(X) >= 0) and r(X) + strict - l(x) > 0) is unsat
-> mu_0 + mu_1*(r(X) - l(x)) + mu_j*p_i(X) = 0
bounded :
forall X . And p_i(X) >= 0 => l(X) > 0
-> forall X . And p_i(X) >= 0 => l(X) - 1 >= 0
-> (And p_i(X) and 1 - l(X) > 0) is unsat


[1] R. Bagnara, F. Mesnard. Eventual Linear Ranking Functions.
[2] A. Podelski and A.Rybalchenko. A Complete Method for the Synthesis of Linear Ranking Functions.
[3] Gulwani, A. Tiwari. Constrainted-based Approach for Analysis of Hybrid Systems.
-}
 
module Tct.Its.Processor.PolyRank  where
  -- ( 
  -- polyRankProcessor
  
  -- , linear
  -- , stronglyLinear
  -- , quadratic
  -- , mixed
  -- , polyDeclaration
  
  -- , farkas
  -- , farkasDeclaration

  -- , timebounds
  -- , timeboundsCandidates

  -- ) where

import           Control.Monad                       (liftM)
import qualified Data.List                           as L (partition, intersect, subsequences, sortOn, reverse)
import           Data.Maybe                          (fromMaybe)
import qualified Data.Map.Strict                     as M
import qualified Data.IntMap.Strict                     as IM
import qualified Data.Set as S 
import qualified Data.Traversable                    as T (mapM)

import           Tct.Core.Common.Error               (throwError)
import qualified Tct.Core.Common.Pretty              as PP
import qualified Tct.Core.Common.Xml                 as Xml
import qualified Tct.Core.Data                       as T

import           Tct.Common.ProofCombinators 
import qualified Tct.Common.Polynomial               as P
import qualified Tct.Common.PolynomialInterpretation as PI
import           Tct.Common.Ring
import qualified Tct.Common.SMT as SMT
-- import qualified SLogic.Smt.Solver (optimathsat)

import qualified Tct.Its.Data.Complexity             as C
import           Tct.Its.Data.Problem
import           Tct.Its.Data.Rule
import           Tct.Its.Data.Types
import qualified Tct.Its.Data.Timebounds             as TB
import qualified Tct.Its.Data.Sizebounds             as SB
import qualified Tct.Its.Data.TransitionGraph        as TG
import           Debug.Trace

--- Instances --------------------------------------------------------------------------------------------------------
poly :: PI.Shape -> ItsStrategy
poly shp = T.Apply polyRankProcessor{ shape = shp}

-- TODO: check exact behaviour if constraints are satisfied
-- in _T2_eric3.koat we get a strict order 1 > 1; which is wrong in the proof
-- yet the rule is not applicable
farkas :: ItsStrategy
farkas = T.Apply polyRankProcessor { useFarkas = True, shape = PI.Linear }

constantFarkas :: ItsStrategy
constantFarkas = T.Apply polyRankProcessor { useFarkas = True, shape = PI.Constant }

timebounds :: [RuleId] -> ItsStrategy
timebounds rs = T.Apply polyRankProcessor { useFarkas = True, shape = PI.Linear, withSizebounds = rs }

timeboundsCandidates :: [RuleId] -> [[RuleId]]
timeboundsCandidates = L.reverse . (L.sortOn length) . tail . L.subsequences -- tail drops [], longer ones first

polyDeclaration ::T.Declaration ('[ T.Argument 'T.Required PI.Shape ] T.:-> ItsStrategy)
polyDeclaration = T.declare "poly" ["(non-linear) polynomial ranking function."] (T.OneTuple PI.shapeArg) poly

farkasDeclaration ::T.Declaration ('[] T.:-> ItsStrategy)
farkasDeclaration = T.declare "farkas" ["linear polynomial ranking function."] () farkas

constant, stronglyLinear, linear, quadratic :: ItsStrategy
constant       = T.Apply polyRankProcessor{ shape = PI.Constant }
stronglyLinear = T.Apply polyRankProcessor{ shape = PI.StronglyLinear }
linear         = T.Apply polyRankProcessor{ shape = PI.Linear }
quadratic      = T.Apply polyRankProcessor{ shape = PI.Quadratic }

mixed :: Int -> ItsStrategy
mixed i = T.Apply polyRankProcessor{ shape = PI.Mixed i }


data PolyRankProcessor = PolyRank
  { useFarkas      :: Bool -- implies linear shape
  , withSizebounds :: [RuleId]
  , shape          :: PI.Shape
  } deriving Show

polyRankProcessor :: PolyRankProcessor
polyRankProcessor = PolyRank
  { useFarkas      = False
  , withSizebounds = []
  , shape          = PI.Linear }

type PolyInter   = PI.PolyInter Fun Int
type IntPoly        = P.Polynomial Int Var
type Coefficient = PI.CoefficientVar Fun

data PolyOrder = PolyOrder
  { shape_   :: PI.Shape
  , pint_    :: PolyInter
  , strict_  :: [(Int,Rule, IntPoly, IntPoly)]
  , weak_    :: [(Int,Rule, IntPoly, IntPoly)]
  , times_   :: IM.IntMap C.Complexity
  , sbounds_ :: Maybe (Vars, SB.Sizebounds)
  } deriving Show

data PolyRankProof = PolyRankProof (OrientationProof PolyOrder) deriving Show

instance PP.Pretty PolyOrder where
  pretty order = PP.vcat $ 
    [ PP.text "We apply a polynomial interpretation of shape" PP.<+> PP.pretty (shape_ order) PP.<> PP.char ':'
    , PP.indent 2 (PP.pretty (pint_ order))
    , PP.text ""
    , PP.text "The following rules are strictly oriented:"
    , ppOrder (PP.text "   > ") (strict_ order)
    , PP.text ""
    , PP.text "The following rules are weakly oriented:"
    , ppOrder (PP.text "  >= ") (weak_ order) ]
    ++ maybe [PP.empty] ppWithSizebounds (sbounds_ order)
    where
      ppWithSizebounds vsbounds = 
        [ PP.text "We use the following global sizebounds:"
        , PP.pretty vsbounds]
      ppOrder ppOrd rs = PP.table [(PP.AlignRight, as), (PP.AlignLeft, bs), (PP.AlignLeft, cs)]
        where
          (as,bs,cs) = unzip3 $ concatMap ppRule rs
          ppRule (_,r, instlhs,instrhs) =
            [ (PP.pretty (con r)              , PP.text " ==> " , PP.empty)
            , (PP.indent 2(PP.pretty (lhs r)) , PP.text "   = " , PP.pretty instlhs)
            , (PP.empty                       , ppOrd           , PP.pretty instrhs)
            , (PP.empty                       , PP.text "   = " , PP.pretty (rhs r))
            , (PP.empty                       , PP.empty        , PP.empty) ]

instance PP.Pretty PolyRankProof where
  pretty (PolyRankProof o) = PP.pretty o

instance Xml.Xml PolyOrder where
  toXml _ = Xml.elt "polynomial" []

instance Xml.Xml PolyRankProof where
  toXml (PolyRankProof o) = Xml.toXml o


instance T.Processor PolyRankProcessor where
  type ProofObject PolyRankProcessor = ApplicationProof PolyRankProof
  type In  PolyRankProcessor         = Its
  type Out PolyRankProcessor         = Its
  type Forking PolyRankProcessor     = T.Optional T.Id

  -- SW: FIXME better? if closedProof returned, no further processors are applied
  execute _ prob | isClosed prob = progress NoProgress (Inapplicable "closed") --closedProof prob 
  execute p prob
    | not (null $ withSizebounds p) && not (sizeIsDefined prob) 
        = progress NoProgress (Inapplicable "Sizebounds not initialised.")
    | otherwise = do
        res  <- entscheide p prob
        uncurry progress =<< case res of
          SMT.Sat order ->
            -- MS: for the timebounds processor we do not enforce that the predecessor of strictcomponents is defined
            if hasProgress prob (times_ order)
              then return (Progress $ updateTimebounds prob (times_ order), Applicable (PolyRankProof (Order order)))
              else return (NoProgress, Applicable (PolyRankProof Incompatible))
          -- MS: should return fail; then it is captured by ErroneousProc
          SMT.Error s   -> throwError (userError s)
          _             -> return (NoProgress, Applicable $ PolyRankProof Incompatible)


newtype Strict = Strict { unStrict :: Int }
  deriving (Eq, Ord, Show)

find :: (Ord k, Show k) => M.Map k a -> k -> a
find m k = error err `fromMaybe` M.lookup k m
  where err = "Tct.Its.Processor.PolyRank: key " ++ show k ++ " not found."

entscheide :: PolyRankProcessor -> Its -> T.TctM (SMT.Result PolyOrder)
entscheide proc prob@Its
  { startterm_       = startterm
  , tgraph_          = tgraph
  , timebounds_      = tbounds
  , sizebounds_      = sizebounds
  } = do
  let 
    solver 
      | useFarkas proc = SMT.yices
      -- | useFarkas proc = SMT.optimathsat
      | otherwise      = SMT.minismt' Nothing ["-m","-ib", "-1"]
  res :: SMT.Result (PolyInter, M.Map Strict Int) <- SMT.smtSolveSt solver $ do 
    SMT.setLogic $ if useFarkas proc then SMT.QF_LIA else SMT.QF_NIA
    -- TODO: memoisation is here not used
    ebsi <- PI.PolyInter `liftM` T.mapM encode absi
    -- (ebsi,coefficientEncoder) <- SMT.memo $ PI.PolyInter `liftM` T.mapM encode absi
    (_, strictVarEncoder) <- SMT.memo $ mapM  (SMT.snvarMO . Strict) is

    let
      strict = (strictVarEncoder `find`) . Strict
      interpretLhs    = interpret ebsi
      interpretRhs ts = bigAdd $ map (interpret ebsi) ts 
      -- FIXME: disjunctions are ignored, one should add disjuncts with scaling
      --        values such that one is 0
      interpretCon cs = [ P.mapCoefficients SMT.num c | [Gte c _] <- toGte cs ]
      absolute p = SMT.bigAnd [ c SMT..== SMT.zero | c <- P.coefficients p ]


    let -- generalised farkas
      decrease (i,Rule l rs cs) = pl `eliminate` interpretCon cs
        where pl = (interpretRhs rs `add` P.constant (strict i)) `sub` interpretLhs l
      bounded (Rule l _ cs) = eliminate pl (interpretCon cs)
        where pl = neg $ interpretLhs l `sub` P.constant one
      eliminate p ps = do
        let nvar' = SMT.nvarM'
        mu0 <- nvar'
        mu1 <- nvar'
        SMT.assert $ mu0 SMT..> SMT.zero SMT..|| mu1 SMT..> SMT.zero
        mui <- mapM (\_ -> P.constant `liftM` nvar') [1..length ps]
        return $ absolute $ bigAdd $ P.constant mu0 : (P.constant mu1 `mul` p) : zipWith mul mui ps

    let -- farkas
      -- TODO: handle non-linear expressions on rhs
      decreaseFarkas (i,Rule l rs cs) = pl `eliminateFarkas` interpretCon (filterLinear cs)
        where pl = interpretLhs l `sub` (interpretRhs rs `add` P.constant (strict i))
      boundedFarkas (Rule l _ cs) = pl `eliminateFarkas` interpretCon (filterLinear cs)
        where pl = interpretLhs l `sub` P.constant one

      eliminateFarkas ply cs = do
        let
          k p = SMT.nvarM' >>= \lambda -> return (lambda `P.scale` p)
        cs2 <- mapM k cs
        let
          (p1,pc1) = P.splitConstantValue ply
          (p2,pc2) = P.splitConstantValue (bigAdd cs2)
        return $ absolute (p1 `sub` p2) SMT..&& (pc1 SMT..>= pc2)


    let
      order (i,r) = do
        fm1 <- if useFarkas proc then decreaseFarkas (i,r) else decrease (i,r)
        fm2 <- if useFarkas proc then boundedFarkas r else bounded r
        return (fm1 SMT..&& ((strict i SMT..> SMT.zero) SMT..=> fm2))

      orderConstraint = [ order r | r <- someirules ]
      rulesConstraint = [ strict i SMT..> SMT.zero | i <- strictrules ]

    let
      undefinedVars = computeUndefinedVars tgraph allrules (IM.fromList someirules) (error "undefvars" `fromMaybe` sizebounds)
      undefinedCofs (Rule l _ _) = [ c | (c,ms) <- pl, (v,_) <- ms, isUndefined (fun l) v ]
        where
          pl = P.toView (interpretLhs l)
          isUndefined f v = case M.lookup f undefinedVars of
            Nothing -> False
            Just vs -> v `elem` vs
      undefinedConstraint 
        | withSize  = SMT.bigAnd [ c SMT..== SMT.zero | r <- somerules, c <- undefinedCofs r ]
        | otherwise = SMT.top
      -- SW: monotonicity (in case of multi-term rhss, coefficients may get negative? avoid)
      inters = [ PI.interpretations ebsi `find` f | f <- funs somerules]
      coeffs = concat [ P.constantValue i : P.coefficients i | i <- inters ]
      -- SW: ensure that condition implies lhs non-negative (condition 3 in [BEFFG16, Def 3.1]) 
      nonneg_lhs (_, (Rule l _ cs)) = (interpretLhs l) `eliminateFarkas` interpretCon cs


    SMT.assert (SMT.top :: SMT.Formula Int)
    SMT.assert =<< SMT.bigAndM orderConstraint
    SMT.assert $ SMT.bigOr rulesConstraint
    SMT.assert $ SMT.maximize $ bigAdd [ strict i | i <- strictrules ]
    SMT.assert undefinedConstraint
    -- SW: if size bounds are used, monotonicity is required; otherwise not
    --     necessarily but lhss must be interpreted non-negative wrt constraints
    -- SW: fix: allow non-monotonicity, but fix afterwards interpretation of term
    -- SMT.assert =<<
    --  if withSize then return (SMT.bigAnd $ [ c SMT..>= SMT.zero | c <- coeffs])
    --  else SMT.bigAndM [] -- nonneg_lhs r | r <- someirules ] but say that vars are positive

    return $ SMT.decode (ebsi, strictVarEncoder)

  return $ mkOrder `fmap` res
  where

    withSize = not $ null (withSizebounds proc)
    allrules = irules_ prob
    someirules
      | withSize  = IM.assocs $ IM.filterWithKey (\k _ -> k `elem` withSizebounds proc)  allrules
      | otherwise = IM.assocs allrules
    (is, somerules) =  unzip someirules
    strictrules = TB.nonDefined tbounds `L.intersect` is
    -- encode = P.fromViewWithM SMT.ivarMO -- FIXME: incorporate restrict var for strongly linear
    encode = P.fromViewWithM enc where
      enc c
        | PI.restrict c = SMT.sivarM'
        | otherwise     = SMT.ivarM'
    absi = M.mapWithKey (curry (PI.mkInterpretation kind)) sig
    kind = PI.Unrestricted shp -- PI.ConstructorBased shp S.empty 
    sig = restrictSignature (S.fromList $ funs somerules) (signature_ prob)
    funs = foldl (\fs (Rule l r _) -> fun l : map fun r ++ fs) []
    shp = shape proc

    interpret ebsi = interpretTerm interpretFun interpretArg
      where
        interpretFun f = P.substituteVariables interp . M.fromList . zip PI.indeterminates
          where interp = PI.interpretations ebsi `find` f
        interpretArg   = P.mapCoefficients SMT.num

    mkOrder :: (PolyInter, M.Map Strict Int) -> PolyOrder 
    mkOrder (PI.PolyInter pint, stricts) = PolyOrder
      { shape_  = shp
      , pint_   = PI.PolyInter pint
      , strict_ = map (\(i,r) -> (i,r, inst (lhs r), bigAdd $ map inst (rhs r))) strictList
      , weak_   = map (\(i,r) -> (i,r, inst (lhs r), bigAdd $ map inst (rhs r))) weakList
      , times_  = IM.fromAscList $ M.toAscList times 
      , sbounds_ = if withSize then (\sb -> (domain prob, sb)) `fmap` sizebounds else Nothing}
      where
        strictMap = M.mapKeysMonotonic unStrict $ M.filter (>0) stricts
        (strictList, weakList) = L.partition (\(i,_) -> i `M.member` strictMap) someirules
        costs
          | withSize = computeBoundWithSize tgraph allrules (IM.fromList someirules) tbounds (error "sizebounds" `fromMaybe` sizebounds) costf 
          | otherwise = C.poly (inst startterm)
          -- SW monotonicity is not required. correct because poly takes abs, thus overestimates
           where costf f = C.poly . inst $ Term f (args startterm)

        times = M.map (const costs) strictMap

        inst = interpretTerm interpretFun interpretArg
        interpretFun f = P.substituteVariables (pint `find` f) . M.fromList . zip PI.indeterminates
        interpretArg a = a

refineBounds :: Its -> PolyOrder -> PolyOrder
refineBounds prob order =
  let
    pint = pint_ order
    weak = weak_ order
    strict = strict_ order
    --by2 = forallM (decrease pint 2) strict
    --order' = PolyOrder {
    --  shape_  = shp order -- not true
    --, pint_   = pint
    --, strict_ = strict
    --, weak_   = weak
    --, times_  = times_ order
    --, sbounds_ = sbounds_ order}
  in
  --if by2 then order else 
  order

computeUndefinedVars :: TG.TGraph -> Rules -> Rules -> SB.Sizebounds -> M.Map Fun [Var]
computeUndefinedVars tgraph allrules somerules sbounds = M.fromList
  [ (f,vs) | (t,i) <- ins, let f = funOf (t,i), let vs = M.keys $ M.filter (==C.Unknown) (SB.boundsOfVars sbounds (t,i))]
  where
    ins   = TG.incoming tgraph (IM.keys somerules) 
    funOf (t,i) = fun . (!! i) . rhs $ allrules IM.! t
  
computeBoundWithSize :: TG.TGraph -> Rules -> Rules -> TB.Timebounds -> SB.Sizebounds -> (Fun -> C.Complexity) -> C.Complexity
computeBoundWithSize tgraph allrules somerules tbounds sbounds prf = bigAdd $ do
  (t,i) <- TG.incoming tgraph (IM.keys somerules)
  let
    innerTBound = prf (fun . (!! i) . rhs $ allrules IM.! t)
    outerTBound = tbounds `TB.tboundOf` t
    innerSBounds = SB.boundsOfVars sbounds (t,i)
  return $ outerTBound `mul` C.compose innerTBound innerSBounds

