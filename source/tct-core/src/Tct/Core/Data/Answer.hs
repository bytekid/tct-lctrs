-- | This module provides standard output formats for a 'Certificate'.
module Tct.Core.Data.Answer
  (
  -- * Generic timebound format.
  Timebounds (..)
  , timebounds
  -- * tttac / termcomp (prior 2015) format
  , TTTAC (..)
  , tttac
  -- * termcomp 2015 format
  , Termcomp (..)
  , termcomp
  ) where


import           Data.Monoid               ((<>))
import qualified Tct.Core.Common.Pretty    as PP
import qualified Tct.Core.Common.Xml       as Xml

import qualified Tct.Core.Data.Certificate as T


-- | @Timebounds lower upper@
data Timebounds = Timebounds T.Complexity T.Complexity

-- | Extracts lower and upper bounds of a certificate.
timebounds :: T.Certificate -> Timebounds
timebounds c@T.Certificate{}    = Timebounds (T.timeLB c) (T.timeUB c)
timebounds T.CertificateYesNo{} = Timebounds T.Unknown T.Unknown

instance PP.Pretty Timebounds where
  pretty (Timebounds lb  ub) = PP.text "Timebounds" <> PP.tupled [PP.pretty lb, PP.pretty ub]

instance Xml.Xml Timebounds where
  toXml (Timebounds lb ub) = Xml.elt "timebounds"
    [ Xml.elt "lowerbound" [Xml.toXml lb]
    , Xml.elt "upperbound" [Xml.toXml ub] ]


--- * termcomp -------------------------------------------------------------------------------------------------------

-- | Newtype wrapper for 'Timebounds'.
-- The pretty printing instance corresponds to the old (prior 2015) /termcomp/ format and is compatible with the
-- /tttac/ testing tool.
data TTTAC = TTTAC Timebounds
           | TTTACYN Bool

-- | Returns the certificate in a /tttac/ compatible format.
--
-- > pretty $ tttac unbounded = "MAYBE"
-- > pretty $ tttac (timeLBCert linear) = "YES(O(n^1),?)"
-- > pretty $ tttac (timeUBCert linear) = "YES(?,O(n^1))"
tttac :: T.Certificate -> TTTAC
tttac c@T.Certificate{}      = TTTAC (timebounds c)
tttac (T.CertificateYesNo x) = TTTACYN x

instance PP.Pretty TTTAC where
  pretty (TTTAC (Timebounds lb  ub))
    | lb /= T.Unknown || ub /= T.Unknown = PP.text "YES" <> PP.tupled [PP.pretty lb, PP.pretty ub]
  pretty (TTTACYN True) = PP.text "YES"
  pretty (TTTACYN False) = PP.text "NO"
  pretty _ = PP.text "MAYBE"

instance Xml.Xml TTTAC where
  toXml (TTTAC (Timebounds lb ub))
    | lb /= T.Unknown || ub /= T.Unknown = Xml.elt "certified"
      [ Xml.elt "lowerbound" [Xml.toXml lb]
      , Xml.elt "upperbound" [Xml.toXml ub] ]
  toXml (TTTACYN True) = Xml.elt "YES" []
  toXml (TTTACYN False) = Xml.elt "NO" []
  toXml _  = Xml.elt "maybe" []


--- * termcomp -------------------------------------------------------------------------------------------------------

-- | Newtype wrapper for 'Timebounds'.
-- The pretty printing instance corresponds to the /termcomp 2015/ format.
-- See <http://cbr.uibk.ac.at/competition/rules.php> (September 2015) for more information.
data Termcomp = Termcomp Timebounds
              | TermcompYN Bool

-- | Returns the certificate in the /termcomp 2015/ format.
--
-- > pretty $ termcomp unbounded = "WORST_CASE(?,?)"
-- > pretty $ termcomp (timeLBCert linear) = "WORSTCASE(Omega(n^1,?)"
-- > pretty $ termcomp (timeUBCert linear) = "WORSTCASE(?,O(n^1)"
termcomp :: T.Certificate -> Termcomp
termcomp c@T.Certificate{}      = Termcomp (timebounds c)
termcomp (T.CertificateYesNo x) = TermcompYN x

-- MAYBE answer, worst case (lb, ub) printout, omega answer, is-nonpoly answer,
-- O notation expression, is-poly answer, unknown answer=?
toTermcomp :: t -> ((t1, t1) -> t) -> (Int -> t1) -> t1 -> (Int -> Int -> t1) -> t1 -> t1 -> Termcomp -> t
toTermcomp maybeA worst omegaA npolyA oA polyA unknownA (Termcomp (Timebounds lb ub)) = case (normlb lb, normub ub) of
  (T.Unknown, T.Unknown) -> maybeA
  (nlb, nub)             -> worst (toclb nlb, tocub nub)
  where
    toclb T.Unknown         = unknownA
    toclb (T.Poly (Just i)) = omegaA i
    toclb _                 = npolyA

    tocub T.Unknown         = unknownA
    tocub (T.Poly (Just i)) = oA 0 i
    tocub (T.LogPoly (Just(i, j))) = oA i j
    tocub _                 = polyA

    normlb p@(T.Poly (Just i)) | i > 0 = p
    normlb e@(T.Exp _)         = e
    normlb lp@(T.LogPoly _)    = lp
    normlb _                   = T.Unknown

    normub p@(T.Poly (Just i)) | i >= 0 = p
    normub p@(T.Poly Nothing)  = p
    normub lp@(T.LogPoly _)    = lp
    normub _                   = T.Unknown
toTermcomp _ _ _ _ _ _ _ _ = error "not possible"

instance PP.Pretty Termcomp where
  pretty (TermcompYN True)  = PP.text "YES"
  pretty (TermcompYN False) = PP.text "NO"
  pretty c@Termcomp{} =
    toTermcomp
      (PP.text "MAYBE")
      (\(lb,ub) -> PP.text "WORST_CASE" <> PP.tupled [lb,ub])
      (\i -> PP.text "Omega" <> PP.parens (PP.text "n^" <> PP.int i))
      (PP.text "NON_POLY")
      (\j i -> -- O(log(n)^j * n^i)
        let
          pexpr = if i == 0 then PP.text "" else PP.text "n^" <> PP.int i
          logexp = if j == 1 then PP.text "" else PP.text "^" <> PP.int j
          lexpr = if j == 0 then PP.text "" else PP.text "log(n)" <> logexp
          conn = PP.text (if i == 0 || j == 0 then "" else "*")
          expr = if i == 0 && j == 0 then PP.char '1' else PP.hcat [lexpr, conn, pexpr]
        in
        PP.char 'O' <> PP.parens expr)
      (PP.text "POLY")
      (PP.char '?')
      c

instance Xml.Xml Termcomp where
  toXml (TermcompYN True)  = Xml.elt "YES" []
  toXml (TermcompYN False) = Xml.elt "NO"  []
  toXml c@Termcomp{} =
    toTermcomp
      (Xml.elt "maybe" [])
      (\(lb,ub) -> Xml.elt "worst_case" [Xml.elt "lowerbound" [lb], Xml.elt "upperbound" [ub]])
      (\i -> Xml.elt "polynomial" [Xml.int i])
      (Xml.text "non_poly")
      (\j i ->
        if j == 0 then Xml.elt "polynomial" [Xml.int i]
        else if i == 0 then Xml.elt "logarithmic" [Xml.int j]
        else Xml.elt "log-poly" [Xml.int j, Xml.int i]
      )
      (Xml.text "poly")
      (Xml.text "unknown")
      c

