module Tct.Its.Data.Rule
  (
  ATerm (..)
  , Term
  , Var
  , Fun
  , mapTerm
  , interpretTerm

  , AAtom  (..)
  , Atom
  , Constraint
  , isLinear
  , filterLinear
  , toGte
  , isSelfLoop
  , celem, celems
  , variables

  , ARule (..)
  , Rule
  , Rules
  , Vars

  , renameWith
  , chain


  -- TODO: move
  , ppSep
  , Parser
  , pRule
  ) where


import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Control.Monad (void)
import Control.Applicative
import qualified Text.Parsec.Expr as PE

import qualified Tct.Common.Polynomial as P
import Tct.Common.Ring
import qualified Tct.Core.Common.Pretty as PP
import qualified Tct.Core.Common.Parser as PR
import           Debug.Trace

import Tct.Its.Data.Types

isLinearA :: Atom -> Bool
isLinearA (Eq p1 p2)  = P.isLinear p1 && P.isLinear p2
isLinearA (Gte p1 p2) = P.isLinear p1 && P.isLinear p2

toGteA :: Ord v => AAtom v -> [AAtom v]
toGteA (Eq p1 p2)  = [Gte (p1 `sub` p2) zero, Gte (p2 `sub` p1) zero]
toGteA (Gte p1 p2) = [Gte (p1 `sub` p2) zero]

toGteD :: Ord v => [AAtom v] -> [[AAtom v]] -- CNF computation
toGteD atoms = 
  let prod_app bss atom = [a : bs | a <- toGteA atom, bs <- bss] in
  foldl prod_app [[]] atoms

toGte :: Ord v => AConstraint v -> AConstraint v
toGte cs = concatMap toGteD cs

interpretTerm :: (Fun -> [a] -> a) -> (IPoly -> a) -> Term -> a
interpretTerm f g t = f (fun t) (map g (args t))

mapTerm :: (f -> f') -> (AIPoly v -> AIPoly v') -> ATerm f v -> ATerm f' v'
mapTerm f g (Term fs as) = Term (f fs) (map g as)

foldTerm :: (a -> IPoly -> a) -> a -> Term -> a
foldTerm f a (Term _ as) = foldl f a as

isLinear :: Constraint -> Bool
isLinear = all (all isLinearA)

filterLinear :: Constraint -> Constraint
filterLinear = filter (all isLinearA)

isSelfLoop :: Rule -> Bool
isSelfLoop r = fun (lhs r) `elem` [fun u | u <- rhs r]

celems :: Constraint -> [IPoly]
celems = concatMap $ concatMap celem

celem :: Atom -> [IPoly]
celem (Eq e1 e2)  = [e1,e2]
celem (Gte e1 e2) = [e1,e2]

mapRule :: (AIPoly v -> AIPoly v') -> ARule f v -> ARule f v'
mapRule f (Rule l r cs) = Rule (mapTerm id f l) (map (mapTerm id f) r) (map (map k) cs)
  where
    k (Eq p1 p2)  = Eq (f p1) (f p2)
    k (Gte p1 p2) = Gte (f p1) (f p2)

foldRule :: (a -> IPoly -> a) -> a -> Rule -> a
foldRule f a (Rule l r cs) = cfold $ rfold $ lfold a
  where
    lfold b = foldTerm f b l
    rfold b = foldl (foldTerm f) b r
    cfold b = foldl f b (celems cs)

-- | unsafe rename
renameWith :: (Ord v', Eq v) => (v -> v') -> ARule f v -> ARule f v'
renameWith f = mapRule (P.rename f)

-- | @rename r1 r2@ renames rule @r2@ wrt to rule @r1@.
rename :: Rule -> Rule -> Rule
rename r1 r2 = rename' (renamer r2)
  where
    renamer = mapRule (P.rename (++ "$"))
    r1vars = variables r1
    rename' r
      | S.null (variables r `S.intersection` r1vars) = r
      | otherwise                               = rename' (renamer r)

variables :: Rule -> S.Set Var
variables = foldRule (\acc r -> acc `S.union` S.fromList (P.variables r)) S.empty


-- | @match r1 r2@ matches the rhs of @r1@ with the lhs of @l2@.
chain :: Rule -> Rule -> Maybe Rule
chain ru1 ru2
  | length (rhs ru1) /= 1 && length (rhs ru2) /= 1 = Nothing -- trace "no chain 2" 
  | all (\r -> fun r /= fun (lhs ru2)) (rhs $ ru1) = Nothing -- trace "no chain 1" 
  | otherwise = Just $ chain' ru1 (rename ru1 ru2)
  where
    chain' (Rule l1 r1 cs1) x@(Rule l2 r2 cs2) = Rule
      { lhs = l1
      , rhs = map (\r -> if fun r == fun (lhs ru2) then mapTerm id subst (head r2) else r) r1
      , con = cs1 ++ map (map (mapCon subst)) cs2 }
      where
        lhsvs = concatMap P.variables (args l2)
        subst1 = M.fromList (map (\v -> (v, P.variable v)) (S.toList $ variables x))
        subst2 = foldl (\m (v,p) -> M.insert v p m) subst1 (zip lhsvs (args (head r1)))
        subst = (`P.substituteVariables` subst2)
        mapCon f (Eq p1 p2)  = Eq (f p1) (f p2)
        mapCon f (Gte p1 p2) = Gte (f p1) (f p2)



-- Pretty Printing ---------------------------------------------------------------------------------------------------
arrowSym, andSym :: String
arrowSym = "->"
andSym   = "&&"
orSym   = "||"


ppTerm :: Term -> PP.Doc
ppTerm (Term f ts) = PP.string f PP.<> PP.tupled (map PP.pretty ts)

ppTerms ::  [Term] -> PP.Doc
ppTerms [t] = ppTerm t
ppTerms ts  = PP.char 'c' PP.<> PP.int (length ts) PP.<> PP.tupled (map ppTerm ts)

instance PP.Pretty Term where
  pretty = ppTerm

instance {-# OVERLAPPING #-} PP.Pretty [Term] where
  pretty = ppTerms


ppBinop :: (PP.Pretty a1, PP.Pretty a2) => a1 -> String -> a2 -> PP.Doc
ppBinop t1 op t2 = PP.pretty t1 PP.<+> PP.text op PP.<+> PP.pretty t2

ppClause :: [Atom] -> PP.Doc
ppClause [] = PP.text "False"
ppClause [a] = PP.pretty a
ppClause as =
  let
    disj = PP.enclose PP.space PP.space (PP.text orSym)
  in
  PP.encloseSep PP.lparen PP.rparen disj (map PP.pretty as)

ppConstr :: [[Atom]] -> PP.Doc
ppConstr [] = PP.text "True"
ppConstr cs =
  let
    conj = PP.enclose PP.space PP.space (PP.text andSym)
  in
  PP.encloseSep PP.lbracket PP.rbracket conj (map ppClause cs)

instance PP.Pretty Atom where
  pretty (Eq t1 t2)  = ppBinop t1 "=" t2
  pretty (Gte t1 t2) = ppBinop t1 ">=" t2

instance {-# OVERLAPPING #-} PP.Pretty [[Atom]] where
  pretty = ppConstr


ppSep :: PP.Doc
ppSep = PP.text arrowSym

instance PP.Pretty Rule where
  pretty (Rule l rs cs) =
    PP.pretty l PP.<+> ppSep PP.<+> ppTerms rs PP.<+> ppConstr cs

-- Parsing -----------------------------------------------------------------------------------------------------------

-- prule should parse
-- f(A,B) -> Com_2(f(A,C),round(A,C)) :|: A >= 1 && B + 1 = A

type Parser = PR.Parsec String ()

pVar :: Parser IPoly
pVar = P.variable <$> PR.identifier

pNat :: Parser IPoly
pNat = P.constant <$> PR.nat

pSep :: Parser ()
pSep = void $ PR.symbol "-" *> weights <* PR.symbol ">"
  where weights = optional $ PR.symbol "{" *> PR.manyTill PR.anyChar (PR.symbol "}")

-- constructs a polynomial over an arbitrary arithmetic expression over: int, var, *, +, -, ()
pPoly :: Parser IPoly
pPoly = PE.buildExpressionParser table poly PR.<?> "poly"
  where
    poly =
      PR.parens pPoly
      PR.<|> pNat
      PR.<|> pVar
    table =
      [ [ unary "-" neg ]
      , [ expn ]
      , [ binaryL "*" mul PE.AssocLeft]
      , [ binaryL "+" add PE.AssocLeft, binaryL "-" sub PE.AssocLeft] ]
    unary f op = PE.Prefix (PR.reserved f *> return op)
    binaryL f op = PE.Infix (PR.reserved f *> return op)
    -- XXX: MS: nasty (partial) hack to parse expressions c*v^n
    expn = PE.Infix (PR.reserved "^" *> return (\p1 p2 -> exp' (P.toView p1) (P.toView p2))) PE.AssocNone
    exp' [(c,[(v,1)])] [(c0,[])] = P.fromView [(c,[(v,c0)])]
    exp' _ _                     = error "can't parse exp"

-- theory term
data TList 
  = CNil
  | CVar [Char]
  | CCons IPoly TList

data TTerm
  = CInt IPoly
  | CList TList

pListV :: Parser TList
pListV = listv PR.<?> "list"
  where
    listv =
      PR.try ((return CNil) <$> PR.symbol "[]")
      PR.<|> (PR.try ((\h _ t -> CCons h t) <$> pPoly <*> (PR.symbol "::") <*> pListV)
      PR.<|> CVar <$> PR.identifier)

pList :: Parser TList
pList = list PR.<?> "list"
  where
    list =
      PR.try ((return CNil) <$> PR.symbol "[]")
      PR.<|> ((\h _ t -> CCons h t) <$> pPoly <*> (PR.symbol "::") <*> pListV)

pTTerm :: Parser TTerm
pTTerm = tterm PR.<?> "tterm"
  where
    tterm = (PR.try (CList <$> pList)) PR.<|> (CInt <$> pPoly)

-- f([poly])
data TheoryTerm = ThTerm 
  { tfun  :: Fun
  , targs :: [TTerm]
  }

data TheoryAtom
  = TEq TTerm TTerm
  | TGte TTerm TTerm

type TheoryConstraint = [[TheoryAtom]]

pTerm :: Parser TheoryTerm
pTerm = (ThTerm <$> PR.identifier <*> PR.parens (pTTerm `PR.sepBy` PR.symbol ",")) PR.<?> "term"


-- Com_nat([terms])
pTerms :: Parser [TheoryTerm]
pTerms = do
  void $ PR.symbol "Com_"
  void PR.nat
  PR.parens(pTerm `PR.sepBy1` PR.symbol ",")
  PR.<?> "terms"

-- poly binop poly (binop: =, >=)
pAtom :: Parser TheoryAtom
pAtom = do
  p1 <- pTTerm
  op <- PR.choice bin
  p2 <- pTTerm
  return $ p1 `op` p2
  PR.<?> "atom"
  where
    bin =
      [ PR.reserved "=" *> return TEq
      , PR.reserved ">=" *> return TGte
      , PR.reserved "=<" *> return (flip TGte)
      , PR.reserved "<=" *> return (flip TGte)
      , PR.reserved ">" *> return (\(CInt p1) p2 -> TGte (CInt (p1 `sub` one))  p2)
      , PR.reserved "<" *> return (\p2 (CInt p1) -> TGte (CInt (p1 `sub` one))  p2) ]

pConstraint :: Parser TheoryConstraint
pConstraint = PR.try pConstraint1 <|> PR.try pConstraint2 <|> return [] PR.<?> "constraint"

-- ( a1 \/ a2 ... )
pClause :: [Char] -> Parser [TheoryAtom]
pClause or_sym = (paren $ pAtom `PR.sepBy` PR.symbol or_sym) <|> (pAtom >>= \a -> return [a])
  where paren p = PR.symbol "(" *> p <* PR.symbol ")"

pClause1 = pClause "\\/"
pClause2 = pClause "||"

-- [ a1 /\ a2 ... ]
pConstraint1 :: Parser TheoryConstraint
pConstraint1 = bracket $ pClause1 `PR.sepBy` PR.symbol "/\\"
  where bracket p = PR.symbol "[" *> p <* PR.symbol "]"

-- :|: a1 && a2 && ..
pConstraint2 :: Parser TheoryConstraint
pConstraint2 = PR.symbol ":|:" *> pClause2 `PR.sepBy` PR.symbol "&&"

measure :: TheoryTerm -> [TheoryTerm] -> TheoryConstraint -> Rule
measure l rs cs =
  let
    terms = targs l ++ (concatMap targs rs) ++ (concatMap (concatMap aterms) cs)
    lterms = [ t | CList t <- terms]
    lconstrs = map add_constr lterms
  in
  Rule (m_term l) (map m_term rs) (map (map m_atom) cs ++ lconstrs)
  where
    m_tterm (CInt p) = p
    m_tterm (CList CNil) = P.constant 0
    m_tterm (CList (CVar x)) = P.variable x
    m_tterm (CList (CCons _ l)) = add (m_tterm (CList l)) (P.constant 1)
    m_term t = Term {fun = (tfun t), args = map m_tterm (targs t)}
    m_atom (TEq s t) = Eq (m_tterm s) (m_tterm t)
    m_atom (TGte s t) = Gte (m_tterm s) (m_tterm t)
    aterms (TEq s t) = [s,t]
    aterms (TGte s t) = [s,t]
    add_constr (CVar x) = [Gte (P.variable x) (P.constant 0)]
    add_constr (CCons _ t) = add_constr t
    add_constr _ = []

pRule :: Parser Rule
pRule = (measure <$> pTerm <*> (pSep *> (pTerms <|> (:[]) <$> pTerm)) <*> pConstraint) PR.<?> "rule"

