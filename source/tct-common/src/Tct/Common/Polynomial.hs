{- | This module provides a tree-based implementation for multivariate polynomials.

A 'Monomial' is well-defined if:

    * each variable (indeterminate) occurs at most once
    * exponents are >= 1

A 'Polynomial' is well-defined if:

    * monomials are well-defined
    * monomials are unique
    * coefficients are non-zero wrt. 'zero'


All operations on 'Polynomial' are invariant wrt. well-definedness.

Polynomials can be constructed using the 'constant' and 'variable' functions together with
standard arithmetic ('SemiRing', 'Ring'); or
using 'PolynomialView', together with 'fromView'.


NOTE: 'Ord' and 'Eq' are derived for the 'Polynomial' and 'Monomial' type.
They are purely syntactical and shouldn't be used to eg compare polynomials.
-}
module Tct.Common.Polynomial
  (
  -- * Polynomial
  Monomial
  , Polynomial
  , scale
  -- ** Getters
  , getCoefficient
  , coefficients
  , coefficients'
  , variables
  , constantValue
  , splitConstantValue
  -- ** construction
  , constant
  , variable
  -- ** Properties
  , degree
  , isLinear
  , isStronglyLinear
  -- ** substitution/maps
  , mapCoefficients
  , mapCoefficientsM
  , rename
  , substituteVariables
  , zipCoefficientsWith

  -- * view
  , PView
  , MView
  , mtoView
  , mfromView
  , toView
  , fromView
  , fromViewWith
  , fromViewWithM
  -- ** view constructors
  , (^^^)
  , constView
  , linear
  , quadratic
  , mixed
  ) where


import           Control.Monad
import           Data.List              (foldl', nub)
import qualified Data.Map.Strict        as M
import           Data.Maybe             (fromMaybe)
import qualified Data.Traversable       as T (mapM)

import qualified Tct.Core.Common.Pretty as PP
import qualified Tct.Core.Common.Xml    as Xml

import           Tct.Common.Ring


--- * monomial -------------------------------------------------------------------------------------------------------

-- | The abstract monomial type.
newtype Monomial v = Mono (M.Map v Int)
  deriving (Eq, Ord, Show)

-- we treat negative exponents as 1
mvariable :: v -> Monomial v
mvariable v = Mono (M.singleton v 1)

mone :: Monomial v
mone = Mono M.empty

mmult :: Ord v => Monomial v -> Monomial v -> Monomial v
mmult (Mono ps1) (Mono ps2) = Mono $ M.unionWith (+) ps1 ps2

instance Ord v => Multiplicative (Monomial v) where
  one = mone
  mul = mmult

mdegree :: Monomial v -> Int
mdegree (Mono ps) = sum (M.elems ps)

misLinear :: Monomial v -> Bool
misLinear m = mdegree m <= 1

-- mvariables :: Monomial v -> [v]
-- mvariables (Mono ps) = M.keys ps


--- * polynomial -----------------------------------------------------------------------------------------------------
-- the zero polynomial is represented by (Poly M.empty)
-- a constant v is represented by (Poly (M.singleton (Mono M.empty) c))

-- | The abstract polynomial type.
newtype Polynomial c v = Poly (M.Map (Monomial v) c)
  deriving (Eq, Ord, Show)

-- | Constructs a polynomial from a constant value.
constant :: (Additive c, Eq c, Ord v) => c -> Polynomial c v
constant c
  | c == zero = pzero
  | otherwise = Poly (M.singleton mone c)

-- | Constructs a polynomial from a variable (with exponent 1).
variable :: (Multiplicative c, Ord v) => v -> Polynomial c v
variable v = Poly (M.singleton (mvariable v) one)

pnormalise :: (Additive c, Eq c) => Polynomial c v -> Polynomial c v
pnormalise (Poly ts) = Poly $ M.filter (/= zero) ts

pzero :: Polynomial c v
pzero = Poly M.empty

padd :: (Additive c, Eq c, Ord v) => Polynomial c v -> Polynomial c v -> Polynomial c v
padd (Poly ts1) (Poly ts2) = pnormalise . Poly $ M.unionWith add ts1 ts2

instance (Additive c, Eq c, Ord v) => Additive (Polynomial c v) where
  zero = pzero
  add  = padd

asConstant :: (SemiRing c, Eq v) => Polynomial c v -> Maybe c
asConstant (Poly ts)
  | M.size ts > 1 = Nothing
  | otherwise     = case M.toList ts of
    []                  -> Just zero
    [(m,c)] | m == mone -> Just c
    _                   -> Nothing

pone :: (Multiplicative c, Ord v) => Polynomial c v
pone = Poly (M.singleton mone one)

-- | Multiplies a constant value with a polynomial.
scale :: (SemiRing c, Eq c) => c -> Polynomial c v -> Polynomial c v
scale c (Poly ts)
  | c == zero = pzero
  | otherwise = Poly $ M.map (`mul` c) ts

pmult :: (SemiRing c, Eq c, Ord v) => Polynomial c v -> Polynomial c v -> Polynomial c v
pmult p1@(Poly ts1) p2@(Poly ts2) = case (asConstant p1, asConstant p2) of
  (Just c1, Just c2) -> constant (c1 `mul` c2)
  (Just c, _)        -> scale c p2
  (_, Just c)        -> scale c p1
  _                  ->
    Poly $ M.fromListWith add
      [ (m1 `mmult` m2, c1 `mul` c2) | (m1,c1) <- M.toList ts1, (m2,c2) <- M.toList ts2 ]

instance (SemiRing c, Eq c, Ord v) => Multiplicative (Polynomial c v) where
  one = pone
  mul = pmult

pnegate :: AdditiveGroup c => Polynomial c v -> Polynomial c v
pnegate (Poly ts) = Poly (M.map neg ts)

instance (AdditiveGroup c, Eq c, Ord v) => AdditiveGroup (Polynomial c v) where
  neg = pnegate



-- | Returns the coefficients of the polynomial (in arbitrary order). Never returns an empty list.
--
-- prop> coefficients (constant zero) = [zero]
coefficients :: (SemiRing c, Ord v) => Polynomial c v -> [c]
coefficients p@(Poly ts)
  | isZero p  = [zero]
  | otherwise = M.elems ts

-- | Returns the coefficient of the given monomial. Returns zero if the monomial does not occur in the polynomial.
getCoefficient :: (Additive c, Ord v) => Polynomial c v -> MView v -> c
getCoefficient (Poly ts) m = zero `fromMaybe` M.lookup (mfromView m) ts

-- | Like 'coefficients', but separates the constant part.
--
-- prop> coefficients' (constant v) = ([], v)
coefficients' :: (SemiRing c, Ord v) => Polynomial c v -> ([c],c)
coefficients' p = (M.elems ts, c)
  where (Poly ts,c) = splitConstantValue p

-- | Returns the (set of) variables of the polynomial (in arbitrary order).
--
-- prop> variables (constant v) = []
variables :: Eq v => Polynomial c v -> [v]
variables (Poly ts) = nub $ concatMap k (M.keys ts)
  where k (Mono ps) = M.keys ps

-- | Returns the constant value of the polynomial.
--
-- prop> constantValue zero = zero
-- prop> constantValue (constant v) = v
constantValue :: (SemiRing c, Ord v) => Polynomial c v -> c
constantValue (Poly ts) = zero `fromMaybe` M.lookup mone ts

-- | Splits a non-zero polynomial in a (non-constant, constant) part.
--
-- prop> splitConstantValue (constant zero) = (zero,zero)
splitConstantValue :: (SemiRing c, Ord v) => Polynomial c v -> (Polynomial c v, c)
splitConstantValue p@(Poly ts)
  | isZero p  = (pzero, zero)
  | otherwise = (Poly (M.filterWithKey f ts), constantValue p)
    where f k _ = k /= mone


--- * Properties -----------------------------------------------------------------------------------------------------

isZero :: Polynomial c v -> Bool
isZero (Poly ts) = M.null ts

-- | Returns the degree of the polynomial.
--
-- prop> degree zero = -1
-- prop> degree (constant v) = 0
degree :: (Additive c, Eq c, Ord v) => Polynomial c v -> Int
degree p@(Poly ts)
  | isZero p  = -1
  | otherwise = maximum (map mdegree $ M.keys ts)

-- | Checks if the polynomial is linear.
isLinear :: Polynomial c v -> Bool
isLinear (Poly ts) = all misLinear (M.keys ts)

-- | Checks if the polynomial is strongly linear.
isStronglyLinear :: (Ring c, Eq c, Ord v) =>  Polynomial c v -> Bool
isStronglyLinear (Poly ts) = all k (M.toList ts)
  where
    k (m, c)
      | m == mone = True
      | otherwise = (c == one || c == neg one) && misLinear m


pbigMult :: (SemiRing c, Eq c, Ord v) => [Polynomial c v] -> Polynomial c v
pbigMult = foldl' pmult pone

pbigAdd :: (SemiRing c, Eq c, Ord v) => [Polynomial c v] -> Polynomial c v
pbigAdd = foldl' padd pzero

-- FIXME: what should be returned for variables not occuring in the mappings
-- consider eg {x->p1} in x*y and {} in x*y; currently we return one so we get p1*1 and 1*1
-- | @'substituteVariables' p subs@ substitutes the variables in p according to @subs@.
-- Variables occuring not in @subs@ are mapped to the unit ('one') polynomial.
substituteVariables :: (SemiRing c, Eq c, Ord v, Ord v') => Polynomial c v -> M.Map v (Polynomial c v') -> Polynomial c v'
substituteVariables (Poly ts) subs = pbigAdd $ foldl' handleTerms [] (M.toList ts)
  where
    handleTerms polys (m,c) = (c `scale` handleMono m) : polys
    subs' = M.toList subs
    handleMono (Mono ps) = pbigMult $ foldl' k [] subs'
      where
        k polys (v,p) = case M.lookup v ps of
          Just i  -> polys ++ replicate i p
          Nothing -> polys

-- | Maps over the coefficients.
mapCoefficients :: (Additive c', Eq c') => (c -> c') -> Polynomial c v -> Polynomial c' v
mapCoefficients f (Poly ts) = pnormalise $ Poly (f `M.map` ts)

-- | Monad version of 'mapCoefficients'.
mapCoefficientsM :: (Monad m, Additive c', Eq c') => (c -> m c') -> Polynomial c v -> m (Polynomial c' v)
mapCoefficientsM f (Poly ts) = (pnormalise . Poly) `liftM` (f `T.mapM` ts)

-- | Unsafe rename.
rename :: (Eq v, Ord v') => (v -> v') -> Polynomial c v -> Polynomial c v'
rename f (Poly ts) = Poly (M.mapKeys k ts)
  where k (Mono ps) = Mono (M.mapKeys f ps)

zipCoefficientsWith :: (Additive c, Eq c, Ord v) => (c -> c -> c) -> Polynomial c v -> Polynomial c v -> Polynomial c v
zipCoefficientsWith f (Poly ts1) (Poly ts2) = pnormalise $ Poly (M.unionWith f ts1 ts2)


--- * View -----------------------------------------------------------------------------------------------------------

type PView c v = [(c,MView v)]
type MView   v = [(v,Int)]

mfromView :: Ord v => MView v -> Monomial v
mfromView = Mono . M.fromListWith add

mtoView :: Monomial v -> MView v
mtoView (Mono ps) = M.assocs ps

fromView :: (Additive c, Eq c, Ord v) => PView c v -> Polynomial c v
fromView = fromViewWith id

-- | prop> toView zero = [(0,[])]
toView :: (Additive c, Eq c, Ord v) => Polynomial c v -> PView c v
toView p@(Poly ts)
  | p == zero = [(zero,[])]
  | otherwise = [ (c, mtoView m) | (m, c) <- M.assocs ts ]

{-coefficientFromView :: MView c v -> c-}
{-coefficientFromView = M.lookup mfromView-}

-- | 'fromViewWith f p' applies @f@ to all coefficients and constructs a polynomial from a view.
--
-- prop> fromViewWith f == mapCoefficients f . fromView (but with no constraints on the original coefficients)
fromViewWith :: (Additive c', Eq c', Ord v) => (c -> c') -> PView c v -> Polynomial c' v
fromViewWith f = Poly . foldl' k M.empty . map (fmap mfromView) where
    k p (c, m) = let c' = f c in
      if c' /= zero then M.insertWith add m c' p else p

-- | Monadic version of 'fromViewWith'.
fromViewWithM :: (Monad m, Additive c', Eq c', Ord v) => (c -> m c') -> PView c v -> m (Polynomial c' v)
fromViewWithM f = liftM Poly . foldM k M.empty . map (fmap mfromView) where
  k p (c,m) = do
    c' <- f c
    return $ if c' /= zero
      then M.insertWith add m c' p
      else p

-- | > v^^^1 = PowV v i
(^^^) :: v -> Int -> (v,Int)
v^^^i = (v,i)

mkTerm :: (a -> b) -> a -> (b, a)
mkTerm f v = (f v,v)

-- | @'constView' f [x,...,z] = c@
-- constructs a constant polynomial; the coefficients are determinded by applying @f@ to each monomial.
constView :: Ord v => (MView v -> c) -> [v] -> PView c v
constView f _ = [mkTerm f []]

-- | @'linear' f [x,...,z] = cx*x + ... + cz*z + c@
-- constructs a linear polynomial; the coefficients are determinded by applying @f@ to each monomial.
linear :: Ord v => (MView v -> c) -> [v] -> PView c v
linear f = (mkTerm f [] :) . map (\v -> mkTerm f [v^^^1])

-- | @'quadratic' f [x,...,z] = cx2*x^2 + cx*x + ... + cz2*z^2 + cz*z + c@
-- constructs a quadratic polynomial; the coefficients are determined by applying @f@ to each monomial.
quadratic :: Ord v => (MView v -> c) -> [v] -> PView c v
quadratic f vs = (mkTerm f [] :) $ map (\v -> mkTerm f [v^^^2]) vs ++ map (\v -> mkTerm f [v^^^1]) vs

-- | Creates a mixed polynom up to a specified degree; the coefficients are determined by applying @f@ to each monomial.
--
-- > mixed 2 (const 1) "xz" = x^2 + x*z + x + z^2 + z + 1
mixed :: Ord v => Int -> (MView v -> c) -> [v] -> PView c v
mixed d f vs = map (mkTerm f) pows
  where
    pows =
      map (filter (\(_,i) -> i>0) . zipWith (\v i -> (v,i)) vs) -- [] isElem of pows
      . filter (\ps -> sum ps <= d)
      . sequence $ replicate (length vs) [0..d]


--- * proofdata ------------------------------------------------------------------------------------------------------

instance PP.Pretty v => PP.Pretty (Monomial v) where
  pretty = ppMonomial PP.pretty

instance (Multiplicative c, Eq c, PP.Pretty c, PP.Pretty v) => PP.Pretty (Polynomial c v) where
  pretty = ppPolynomial PP.pretty PP.pretty

ppMonomial :: (v -> PP.Doc) -> Monomial v -> PP.Doc
ppMonomial pp (Mono ps)
  | M.null ps = PP.empty
  | otherwise = PP.hcat . PP.punctuate (PP.char '*') . map k $ M.toAscList ps
    where k (v,i) = pp v PP.<> if i == 1 then PP.empty else PP.char '^' PP.<> PP.int i

ppPolynomial  :: (Multiplicative c, Eq c) => (c -> PP.Doc) -> (v -> PP.Doc) -> Polynomial c v -> PP.Doc
ppPolynomial ppr ppv p@(Poly ts)
  | isZero p  = PP.int 0
  | otherwise = PP.hcat . PP.punctuate (PP.text " + ") $ map k $ M.toAscList ts
  where
    k (m@(Mono ps), c)
      | M.null ps = ppr c
      | c == one  = ppMonomial ppv m
      | otherwise = ppr c PP.<> PP.char '*' PP.<> ppMonomial ppv m

instance Enum v => Xml.Xml (Polynomial Int v) where
  toXml p = Xml.elt "polynomial" . (:[]) $ xmlPolynomial
    (\c -> Xml.elt "coefficient" [Xml.elt "integer" [Xml.int c]])
    (\v -> Xml.elt "variable" [Xml.int $ fromEnum v])
    p
  toCeTA = Xml.toXml

xmlPolynomial :: Additive c => (c -> Xml.XmlContent) -> (v -> Xml.XmlContent) -> Polynomial c v -> Xml.XmlContent
xmlPolynomial xc xv p@(Poly ts)
  | isZero p  = xc zero
  | otherwise = xop "sum" (map xmono $ M.toList ts)
  where
    xop s as = Xml.elt s [ Xml.elt "polynomial" [a] | a <- as]
    xmono (Mono ps, c)
      | M.null ps = xc c
      | otherwise = xop "product" (xc c: concatMap xpows (M.toList ps))
    xpows (v,i) = replicate i (xv v)

