module Tct.Its.Processor.RestrictVars
  ( restrict
  ) where

import           Control.Monad
import qualified Data.Map.Strict              as M
import qualified Data.IntMap.Strict           as IM

import qualified Tct.Core.Common.Pretty       as PP
import qualified Tct.Core.Common.Xml          as Xml
import qualified Tct.Core.Data                as T

import           Tct.Common.ProofCombinators
import           Tct.Common.Ring

import           Tct.Its.Data.Problem
import           Tct.Its.Data.Rule
import qualified Tct.Its.Data.Timebounds      as TB
import qualified Tct.Its.Data.Sizebounds      as SB
import qualified Tct.Its.Data.TransitionGraph as TG
import           Tct.Its.Data.Types
import           Tct.Its.Data.Complexity
import qualified Tct.Common.Polynomial        as P
import           Debug.Trace


data RestrictVarsProcessor = RestrictVarsProcessor [RuleId]
  deriving Show

data RestrictVarsProof
  = RestrictVarsProof
    { removedArgs  :: [Var] }
  | NoRestrictVarsProof
  deriving Show

instance PP.Pretty RestrictVarsProof where
  pretty NoRestrictVarsProof
    = PP.text "No variables could be removed."
  pretty pproof = PP.hsep
    [ PP.text "We removed the arguments"
    , PP.pretty (removedArgs pproof)
    , PP.dot ]

instance Xml.Xml RestrictVarsProof where
  toXml NoRestrictVarsProof = Xml.elt "norestrictvars" []
  toXml _                  = Xml.elt "restrictvars" []

instance T.Processor RestrictVarsProcessor where
  type ProofObject RestrictVarsProcessor = ApplicationProof RestrictVarsProof
  type In  RestrictVarsProcessor         = Its
  type Out RestrictVarsProcessor         = Its
  type Forking RestrictVarsProcessor     = T.Optional T.Id

  execute (RestrictVarsProcessor choice) prob =
    case restrict prob of
      Nothing              -> progress NoProgress (Applicable NoRestrictVarsProof)
      Just (nprob, pproof) -> progress (Progress nprob) (Applicable pproof)

restrict :: Its -> Maybe (Its, RestrictVarsProof)
restrict prob =
  let
    rules = IM.elems (irules_ prob)
    rlvars rl = map (head . P.variables) (args (lhs rl))
    irulesargs = [ (rl, rlvars rl) | rl <- rules]
    allvars = snd (head irulesargs)
    same_vars =  all (\(_, a) -> a == allvars) (tail irulesargs)
    polys (Eq a b) = [a,b]
    polys (Gte a b) = [a,b]
    deleteAt idx xs = lft ++ rgt
      where (lft, (_:rgt)) = splitAt idx xs
    used (i,x) = let
        in_constr c = x `elem` (concat (map P.variables (concat (map polys c))))
        rhs_use t = x `elem` (concat (map P.variables (deleteAt i (args t))))
      in
      any (\rl -> in_constr (concat (con rl)) || any rhs_use (rhs rl)) rules 
    removedvars = filter (not . used) (zip [0..] allvars)
    vars_idxs = filter used (zip [0..] allvars)
    idxs = map fst vars_idxs
    vars = map snd vars_idxs
    restrictTerm t = Term { fun = fun t, args = map snd (filter (\(i,_) -> i `elem` idxs) (zip [0..] (args t)))}
    restrictRule rl = Rule {
        lhs = Term {fun = fun (lhs rl), args = map P.variable vars}
      , rhs = map restrictTerm (rhs rl)
      , con = con rl }
    newrules = map restrictRule rules
    nprob = initialise ([fun (startterm_ prob)], vars, newrules)
  in
  if not same_vars || removedvars == [] then Nothing
  else Just (nprob, RestrictVarsProof (map snd removedvars))