{-# LANGUAGE ScopedTypeVariables #-}

module Tct.Its.Processor.LoopRecurrence
  ( loopAnalysis
  , maxLoopCost
  , maxLoopOuts
  ) where

import           Control.Monad
import qualified Data.List                           as L (partition, intersect, subsequences, nub, all, transpose)
import           Data.Maybe                          (fromMaybe)
import qualified Data.Map.Strict                     as M
import qualified Data.IntMap.Strict                  as IM
import qualified Data.Set as S 
import qualified Data.Traversable                    as T (mapM)
import           Data.Foldable                       (toList)
import           Data.Typeable                       (cast)

import           Tct.Core.Common.Error               (throwError)
import qualified Tct.Core.Common.Pretty              as PP
import qualified Tct.Core.Common.Xml                 as Xml
import qualified Tct.Core.Data                       as T
import           Tct.Core                            (evaluate1)
import           Tct.Common.Ring

import           Tct.Common.ProofCombinators
import           Tct.Its.Data.Problem
import           Tct.Its.Data.Rule                   (lhs, rhs, variables, fun, args)
import           Tct.Its.Data.Types
import qualified Tct.Its.Data.Complexity as C 

import qualified Tct.Common.Polynomial               as P

import qualified Tct.Its.Data.Timebounds             as TB
import qualified Tct.Its.Data.Sizebounds             as SB
import qualified Tct.Its.Data.TransitionGraph        as TG
import qualified Tct.Core.Common.Pretty              as PP
import           Tct.Its.Processor.PolyRank          (farkas)
import           Tct.Its.Processor.Sizebounds        (sizebounds)
import           Tct.Its.Processor.Simplification    (boundTrivialSCCs, knowledgePropagation)
import           Tct.Its.Processor.Empty             (empty)
import           Tct.Core.Data.Strategy              (try, te, withProblem)
import           Tct.Its.Data.Selector               (selNextSCCAny)

import           Tct.Common.SMT ((.&&), (.=>), (.+), (.*), (.-))
import qualified Tct.Common.SMT as SMT

--import           Debug.Trace
trace _ expr = expr

type IntPoly        = P.Polynomial Int Var
type SMTExpr        = SMT.IExpr Var

data LoopRecurrenceProcessor = LoopRecurrenceProcessor [RuleId]
  deriving Show

data ArithExpr
  = AVar Var
  | AConst Int
  | APlus ArithExpr ArithExpr
  | AMinus ArithExpr ArithExpr
  | AMult ArithExpr ArithExpr
  | ADiv ArithExpr ArithExpr -- 2nd arg actually constant or variable
  deriving (Eq)

instance PP.Pretty ArithExpr where pretty a = ppArith a
ppArith :: ArithExpr  -> PP.Doc
ppArith (AVar x) = PP.text x
ppArith (AConst c) = PP.int c
ppArith (APlus e1 e2) = PP.hcat [ppArith e1, PP.char '+', ppArith e2]
ppArith (AMinus e1 e2) = PP.hcat [ppArith e1, PP.char '-', ppArith e2]
ppArith (AMult e1 e2) = PP.hcat [ppArith e1, PP.char '*', ppArith e2]
ppArith (ADiv e c) = PP.hcat [ppArith e, PP.char '/', ppArith c]

instance Show ArithExpr where show a = arithStr a
arithStr :: ArithExpr -> String
arithStr (AVar x) = x
arithStr (AConst c) = show c
arithStr (APlus a b) = "(" ++ arithStr a ++ " + " ++ arithStr b ++ ")"
arithStr (AMinus a b) = "(" ++ arithStr a ++ " - " ++ arithStr b ++ ")"
arithStr (AMult a b) = "(" ++ arithStr a ++ " * " ++ arithStr b ++ ")"
arithStr (ADiv a c) = "(" ++ arithStr a ++ " / " ++ arithStr c ++ ")"

instance Additive ArithExpr where
  zero = AConst 0
  add  = APlus

data PolyLogExpr 
  = Var Var
  | Add PolyLogExpr PolyLogExpr
  | Mult PolyLogExpr PolyLogExpr
  | Log Int Var
  | Const Int
  deriving (Eq, Ord, Show)

instance PP.Pretty PolyLogExpr where pretty a = ppPolyLog a
ppPolyLog :: PolyLogExpr  -> PP.Doc
ppPolyLog (Var x) = PP.text x
ppPolyLog (Const c) = PP.int c
ppPolyLog (Add e1 e2) = PP.hcat [ppPolyLog e1, PP.char '+', ppPolyLog e2]
ppPolyLog (Mult e1 e2) = PP.hcat [ppPolyLog e1, PP.char '*', ppPolyLog e2]
ppPolyLog (Log b e) = PP.hcat [PP.int b, PP.text "log(", PP.text e, PP.char ')']

data Recurrence = R
  { recurrencePattern :: String -- FIXME more details?
  , expr :: ArithExpr 
  , solution  :: C.Complexity
  } deriving Show
  
data LoopRecurrenceProof
  = LoopRecurrenceProof Recurrence
  | NoLoopRecurrenceProof
  deriving Show

instance PP.Pretty LoopRecurrenceProof where
  pretty NoLoopRecurrenceProof
    = PP.text "No solvable loop recurrence found."
  pretty (LoopRecurrenceProof (R p e s)) = PP.hsep
    [ PP.text "Applying the recurrence pattern"
    , PP.text p
    , PP.text "to the expression"
    , PP.pretty e
    , PP.text "yields the solution"
    , PP.pretty s
    , PP.dot ]

instance Xml.Xml LoopRecurrenceProof where
  toXml NoLoopRecurrenceProof = Xml.elt "norecurrence" []
  toXml _            = Xml.elt "recurrence" []

instance T.Processor LoopRecurrenceProcessor where
  type ProofObject LoopRecurrenceProcessor = ApplicationProof LoopRecurrenceProof
  type In  LoopRecurrenceProcessor         = Its
  type Out LoopRecurrenceProcessor         = Its
  type Forking LoopRecurrenceProcessor     = T.Optional T.Id

  --execute _ prob | isClosed prob = closedProof prob
  execute (LoopRecurrenceProcessor rs) prob =
    loopCheck prob rs >>= \b -> case b of
    Nothing              -> progress NoProgress (Applicable NoLoopRecurrenceProof)
    Just (nprob, pproof) -> progress (Progress nprob) (Applicable pproof)

loopAnalysis :: String -> [RuleId] -> ItsStrategy
loopAnalysis s = trace ("loopAnalysis " ++ s) (T.Apply . LoopRecurrenceProcessor)

loopCheck :: Its -> [RuleId] -> T.TctM (Maybe (Its, LoopRecurrenceProof))
loopCheck prob rs =
  case (rs, sizebounds_ prob) of 
    ([rid], Just sbounds) -> loopCheck' (trace ("go into loop check " ++ show (TB.totalBound (timebounds_ prob))) prob) rid sbounds
    _ -> return (trace ("loop analysis seems to require chaining " ++ show rs ++ " " ++ (show (sizebounds_ prob == Nothing))) Nothing) -- FIXME: also support the case without chaining? (but sizebounds are needed)


loopCheck' :: Its -> RuleId -> SB.Sizebounds -> T.TctM (Maybe (Its, LoopRecurrenceProof))
loopCheck' prob rid sbounds = do
  let
    rule = trace ("loop analysis: found single-rule loop " ++ show rid) (irules_ prob IM.! rid)
    largs = args (lhs rule)
    lvars = L.nub (concat (map P.variables largs))
    rec_calls :: [Term] = filter (\r -> fun r == fun (lhs rule)) (rhs rule)
    rest_rhs = filter (\r -> fun r /= fun (lhs rule)) (rhs rule)
  addtbnds <- solveSubproblem prob rest_rhs (con rule)
  res <- case (trace ("subproblem complexity " ++ show addtbnds) addtbnds) of
    Just (add_bound, reach_rls) ->
      --FIXME pass invariant of SCC instead of true constraint
      case findInitCond [] (con rule) of
        Nothing -> return Nothing
        Just (x, c) ->
          let
            rexprs = map (\t -> map (arithSimp . poly2arith) (args t)) rec_calls
            lexprs = map (arithSimp . poly2arith) largs
            arg_pairs = zip lexprs rexprs
          in
          solveRecurrence lexprs rexprs x add_bound (con rule) >>= \recur ->
          case recur of
            Just solvedRecurrence -> 
              let
                tg = tgraph_ prob
                tbounds = timebounds_ prob
                sol = solution solvedRecurrence
                compl_sum = sumLoopComplexityWithSize tg [rid] tbounds sbounds sol
                prf = LoopRecurrenceProof solvedRecurrence
                tb = foldl (\tb r -> TB.update r compl_sum tb) tbounds (rid : reach_rls)
                prob' = trace ("proof found: " ++ show prf) (prob { timebounds_ = tb })
              in
              return (Just (prob', prf))
            _ -> return Nothing
    _ -> return (trace "loop analysis: unsolved subproblem" Nothing) -- FIXME more than one rhs
  return res
  
sumLoopComplexityWithSize :: TG.TGraph -> [RuleId] -> TB.Timebounds -> SB.Sizebounds -> C.Complexity -> C.Complexity
sumLoopComplexityWithSize tgraph somerules tbounds sbounds compl = bigAdd $ do
  (t,i) <- TG.incoming tgraph somerules
  let
    outerTBound = tbounds `TB.tboundOf` t
    innerSBounds = SB.boundsOfVars sbounds (t,i)
    composed = C.compose compl innerSBounds
  return (outerTBound `mul` (trace ("comp is " ++ show composed) composed))

data Growth = Arbitrary | ConstantGrowth | Sublinear | Linear

data RecurrencePattern = P {
  name :: String
, mkReductExpr :: ArithExpr -> ArithExpr
, branch :: Int
, growth :: Growth
, margin :: Int
, mkSolution  :: IPoly -> C.Complexity -> C.Complexity}

mk_pattern n e b g m s = P {
  name = n,
  mkReductExpr = e,
  branch = b,
  growth = g,
  margin = m,
  mkSolution = s}

solveRecurrence :: [ArithExpr] -> [[ArithExpr]] -> Var -> C.Complexity -> Constraint -> T.TctM (Maybe Recurrence)
solveRecurrence lexprs rexprss x inner_complexity constr =
  if not (all (\e -> case e of {AVar _ -> True; _ -> False}) lexprs) then return Nothing
  else
    let
      trexprss = L.transpose rexprss
      -- 3 types of decreasing expressions: variables, sums, differences
      es = zip lexprs trexprss
      sums = [ (APlus x y, [APlus x' y' | (x', y') <- zip xs ys]) | (x,xs) <- es, (y,ys) <- es, x /= y ]
      diffs = [ (AMinus x y, [AMinus x' y' | (x', y') <- zip xs ys]) | (x,xs) <- es, (y,ys) <- es, x /= y ]
      exprs = es ++ diffs -- ++ sums -- potentially reductive expressions
      -- hardcode some recursion patterns
      log e = C.LogPoly e 1 (P.constant 1) (P.constant 0)
      nlogn e = C.LogPoly e 1 e (P.constant 0)
      times2 e = AMult e (AConst 2)
      rec_patterns = [
        mk_pattern "log" times2 1 ConstantGrowth 1 (\e _ -> log e),
        mk_pattern "Master Theorem (A)" times2 2 Sublinear 1 (\e _ -> C.NPoly e),
        mk_pattern "Master Theorem (B)" times2 2 Linear 1 (\e _ -> nlogn e),
        mk_pattern "log * f" times2 1 Arbitrary 1 (\e f -> log e `mul` f),
        mk_pattern "linear tree * f" times2 2 Arbitrary 1 (\e f -> C.NPoly e `mul` f),
        mk_pattern "linear * f" (\e -> (APlus e (AConst 1))) 1 Arbitrary 0 (\e f -> C.NPoly e `mul` f)
        ]
    in
    foldM check_decrease Nothing [(p,r) | p <- exprs, r <- rec_patterns]
    where
      check_decrease j@(Just _) _ = return j
      check_decrease Nothing p = checkDecreasePattern p constr inner_complexity

checkDecreasePattern :: ((ArithExpr, [ArithExpr]), RecurrencePattern) -> Constraint -> C.Complexity -> T.TctM (Maybe Recurrence)
checkDecreasePattern ((lexpr, rexprs), p) rule_constr inner_complexity = do
  let
    -- FIXME: can we SMT solve for the branching factor, to avoid fixing it?
    lsmt = arith2SMT (APlus lexpr (AConst (margin p)))
    smt_rule_constr = SMT.bigAnd (map (SMT.bigOr . (map encodeAtom)) rule_constr)
    rlen = length rexprs
    lvars = P.variables (arith2poly lexpr)
    -- check whether growth rate of inner complexity is in bounds of pattern
    growth_ok = case growth p of
      ConstantGrowth  -> all (C.isConstantIn inner_complexity) lvars
      Linear          -> all (C.isLinearIn inner_complexity) lvars
      Sublinear       -> C.isLog inner_complexity ||
                         all (C.isConstantIn inner_complexity) lvars
      _               -> True
  all_decrease <- foldM (\decreasing rexpr ->
    if not decreasing then return False
    else do
      let
        redexpr = mkReductExpr p (trace (" (left,right) = (" ++ show lexpr ++ ", " ++ show rexpr ++ ")") rexpr)
        weak_decrease = lsmt SMT..>= (arith2SMT (trace ("redexpr " ++ (show redexpr)) redexpr))
        cconstr = smt_rule_constr SMT..&& SMT.bnot weak_decrease
      --constr_sat <- isSAT (smt_rule_constr SMT..&& (lsmt SMT..>= SMT.zero))
      constr_sat <- isSAT (smt_rule_constr)
      may_be_neg <- isSAT (smt_rule_constr SMT..&& SMT.bnot (lsmt SMT..>= SMT.zero))
      -- rule constraint && no weak decrease: should be unsat, to obtain desired decrease
      no_decr_unsat <- isSAT cconstr
      let
        res = constr_sat && not no_decr_unsat && not may_be_neg
      return (trace ("  constr_sat " ++ show constr_sat ++ " not no_decr_unsat " ++ show (not no_decr_unsat) ++ " may be neg " ++ show may_be_neg) res)
    ) (trace ("try pattern " ++ (name p) ++ " for " ++ show lexpr ++ ": " ++ show growth_ok) growth_ok) rexprs
  return (
    if not all_decrease || rlen > branch p then Nothing
    else
      let inner_complexity_nz = if C.isZero inner_complexity then C.poly (P.constant 1) else inner_complexity in
      Just (R {
      recurrencePattern = name p, 
      expr = lexpr, 
      solution = mkSolution p (arith2poly lexpr) inner_complexity_nz})
    )

isSAT :: SMT.Formula Var -> T.TctM Bool
isSAT f = do
  s :: SMT.Result () <- SMT.smtSolveSt SMT.yices $ do
    SMT.setLogic SMT.QF_NIA
    SMT.assert f
    return $ SMT.decode ()
  return $ SMT.isSat s

solveSubproblem :: Its -> [Term] -> Constraint -> T.TctM (Maybe (C.Complexity, [RuleId]))
solveSubproblem p ts constr = 
  let 
    zero = Just (C.poly (P.constant 0), [])
  in
  foldM (\c t -> solveSubproblem' p t constr >>= \r -> case (r, c) of
    (Nothing, _) -> return Nothing
    (_, Nothing) -> return Nothing
    (Just (c1, rls1), Just (c2, rls2)) -> return (Just (add c1 c2, rls1 ++ rls2))
    ) zero ts

solveSubproblem' :: Its -> Term -> Constraint -> T.TctM (Maybe (C.Complexity, [RuleId]))
solveSubproblem' prob t constr =
  let
    irules = irules_ prob
    init = filter (\r -> fun (lhs (irules IM.! r)) == fun t) (IM.keys irules)
    reachable_ids = TG.descendants (tgraph_ prob) init
    reachable_rls = map (irules IM.!) reachable_ids
    vars = L.nub (concat [concatMap P.variables (args (lhs r)) | r <- reachable_rls])
    start_sym = "_start"
    start_lhs = Term {fun = start_sym, args = map P.variable vars}
    start_rule = Rule {lhs = start_lhs, rhs = [t], con = constr}
    subprob = initialise ([start_sym], vars, trace ("start " ++ show start_lhs ++ " vars: " ++ show vars) start_rule : reachable_rls)
    simple_strategy =
      try sizebounds
      T..>>> te (farkas False)
      T..>>> try (withProblem $ \prob -> loopAnalysis "subproblem" (selNextSCCAny prob))
      T..>>> try knowledgePropagation
  in
    evaluate1 simple_strategy subprob >>= answer reachable_ids
    where
      -- walk down to leaf to obtain all time bounds
      answer rls t@(T.Success pn _ pts) | T.isClosed t = 
        case toList pts of
            []   -> case (cast (T.problem pn) :: Maybe Its) of
              Just p  -> 
                return $ Just (TB.totalBound (timebounds_ p), rls)
              Nothing -> return Nothing
            [pt] -> answer rls pt
            _    -> return (trace ("LoopRecurrence.subProblem: CHECK ME") Nothing)
      answer _ _ = return (trace "subproblem not closed" Nothing)

arithSubst :: (M.Map Var ArithExpr) -> ArithExpr -> ArithExpr
arithSubst sub v@(AVar x) = if M.member x sub then sub M.! x else v
arithSubst sub c@(AConst _) = c
arithSubst sub (APlus e1 e2) = APlus (arithSubst sub e1) (arithSubst sub e2)
arithSubst sub (AMinus e1 e2) = AMinus (arithSubst sub e1) (arithSubst sub e2)
arithSubst sub (AMult e1 e2) = AMult (arithSubst sub e1) (arithSubst sub e2)
arithSubst sub (ADiv e c) = ADiv (arithSubst sub e) (arithSubst sub c)

arithVars :: ArithExpr -> [Var]
arithVars (AVar x) = [x]
arithVars (AConst _) = []
arithVars (APlus e1 e2) = (arithVars e1) ++ (arithVars e2)
arithVars (AMinus e1 e2) = (arithVars e1) ++ (arithVars e2)
arithVars (AMult e1 e2) = (arithVars e1) ++ (arithVars e2)
arithVars (ADiv e _) = (arithVars e)

arithSimp1 :: ArithExpr -> ArithExpr
arithSimp1 e =
  case e of
    APlus (AConst c) a | c == 0 -> arithSimp a
    APlus a (AConst c) | c == 0 -> arithSimp a
    AMult a (AConst c) | c == 1 -> arithSimp a
    AMult (AConst c) a | c == 1 -> arithSimp a
    ADiv a (AConst c) | c == 1 -> arithSimp a
    APlus a b -> APlus (arithSimp a) (arithSimp b)
    AMult a b -> AMult (arithSimp a) (arithSimp b)
    ADiv a c -> ADiv (arithSimp a) (arithSimp c)
    _ -> e
arithSimp :: ArithExpr -> ArithExpr
arithSimp e = let e' = arithSimp1 e in if e == e' then e else arithSimp e'

solve :: Constraint -> Var -> Maybe ArithExpr
solve con x =
  foldl (\sol atom -> case sol of {Nothing -> check atom; _ -> sol}) Nothing con
  where
    check [Eq p1 p2] = solvePolys x p1 p2
    check _ = Nothing

solvePolys :: Var -> IntPoly -> IntPoly -> Maybe ArithExpr
solvePolys x p1 p2 =
  if x `elem` (P.variables p1) then
    if x `elem` (P.variables p2) then Nothing -- FIXME: might still be solvable
    else solve_right (poly2arith p2) p1
  else
    if x `elem` (P.variables p2) then solve_right (poly2arith p1) p2
    else Nothing
  where
    solve_right parith p =
      let
        ml = P.toView p
        (mx,ms) = L.partition (\cm -> x `elem` P.variables (P.fromView [cm])) ml
        parith' = APlus parith (poly2arith (P.scale (-1) (P.fromView ms)))
      in case mx of
        [(c, [(y, e)])] | x == y && e == 1 -> Just (arithSimp (ADiv parith' (AConst c)))
        _ -> Nothing

poly2arith :: IntPoly -> ArithExpr
poly2arith p =
  let
    plist = P.toView p
  in
  foldl (\a (c,m) -> APlus a (AMult (m2arith m) (AConst c))) (AConst 0) plist
  where
    m2arith ms = foldl (\a (vm,c) -> (AMult a (v2arith vm c))) (AConst 1) ms
    v2arith x n =
      if n == 0 then AConst 1 else AMult (AVar x) (v2arith x (n - 1))

arith2poly :: ArithExpr -> IPoly
arith2poly p = P.fromView  (arith2poly' p)
arith2poly' (AVar x) = [(1, [(x,1)])]
arith2poly' (AConst c) = [(c, [])]
arith2poly' (APlus e1 e2) = (arith2poly' e1) ++ (arith2poly' e2)
arith2poly' (AMinus e1 e2) =
  (arith2poly' e1) ++ P.toView (P.scale (-1) (P.fromView (arith2poly' e2)))
--arith2poly' (AMult e1 e2) = (arith2SMT e1) .* (arith2SMT e2)
  
arith2SMT :: ArithExpr -> SMTExpr
arith2SMT (AVar x) = SMT.ivar x
arith2SMT (AConst c) = SMT.num c
arith2SMT (APlus e1 e2) = (arith2SMT e1) .+ (arith2SMT e2)
arith2SMT (AMinus e1 e2) = (arith2SMT e1) .- (arith2SMT e2)
arith2SMT (AMult e1 e2) = (arith2SMT e1) .* (arith2SMT e2)
-- arith2SMT (ADiv e1 e2) = (arith2SMT e1) -- FIXME wrong

-- FIXME: first argument should be entry condition of SCC 
findInitCond :: Constraint -> Constraint -> Maybe (Var, ArithExpr)
findInitCond _ constr  =
  foldl (\sol a -> case sol of {Nothing -> check_disj a; _ -> sol}) Nothing constr
  where
    check_disj (a : ats) = foldl (\s a -> case s of 
      Just (x,e) -> case check a of {Just (x', e') | x == x' -> Just (x, APlus e e'); _ -> Nothing}
      _ -> Nothing) (check a) ats
    check_disj [] = Nothing
    check (Gte p1 p2) = 
      -- FIXME check constr0 implying c, try more variables
      case P.variables (P.fromView (P.toView p1 ++ P.toView p2)) of
        x : _ -> case solvePolys x p1 p2 of {Just e -> Just (x,e); _ -> Nothing}
        [] -> Nothing
    check (Eq _ _) = Nothing
      

maxLoopCost :: Int -> Its -> RuleId -> Bool
maxLoopCost n prob r = TB.tcostOf (timebounds_ prob) r <=  n

maxLoopOuts :: Int -> Its -> RuleId -> Bool
maxLoopOuts n prob r = null $ drop n (TG.successors (tgraph_ prob) r)
