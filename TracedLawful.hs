{-# LANGUAGE GADTs #-}

-- Traces belong to values. A product holds two independently traced values,
-- and unit holds no trace, so projections and terminal discard the provenance
-- of outputs they discard.
module TracedLawful
  ( HasProvenance (..)
  , TracedLawful (..)
  , valueOf
  , traceOf
  , runWithLawful
  ) where

import Control.Category (Category (..))
import Main
  ( Cartesian (..), Event (..), Format, Nat, ParseError, ParseResult (..)
  , Trace (..), parTrace, predecessor, seqTrace
  )
import Prelude hiding ((.), id)

data HasProvenance a where
  PUnit :: HasProvenance ()
  PPair :: HasProvenance a -> HasProvenance b -> HasProvenance (a, b)
  PNat :: Nat -> Trace -> HasProvenance Nat
  PBool :: Bool -> Trace -> HasProvenance Bool
  PChar :: Char -> Trace -> HasProvenance Char
  PString :: String -> Trace -> HasProvenance String
  PFormat :: Format -> Trace -> HasProvenance Format
  PResult :: Either ParseError ParseResult -> Trace
          -> HasProvenance (Either ParseError ParseResult)
  PNatList :: [Nat] -> Trace -> HasProvenance [Nat]

valueOf :: HasProvenance a -> a
valueOf PUnit = ()
valueOf (PPair a b) = (valueOf a, valueOf b)
valueOf (PNat n _) = n
valueOf (PBool b _) = b
valueOf (PChar c _) = c
valueOf (PString s _) = s
valueOf (PFormat format _) = format
valueOf (PResult result _) = result
valueOf (PNatList ns _) = ns

-- This is a convenient summary for observation, not the internal
-- representation of a product's provenance.
traceOf :: HasProvenance a -> Trace
traceOf PUnit = Empty
traceOf (PPair a b) = parTrace (traceOf a) (traceOf b)
traceOf (PNat _ trace) = trace
traceOf (PBool _ trace) = trace
traceOf (PChar _ trace) = trace
traceOf (PString _ trace) = trace
traceOf (PFormat _ trace) = trace
traceOf (PResult _ trace) = trace
traceOf (PNatList _ trace) = trace

-- A selected branch depends on its condition. Distribute that dependency to
-- every surviving output component; unit has no provenance to receive it.
prefixTrace :: Trace -> HasProvenance a -> HasProvenance a
prefixTrace _ PUnit = PUnit
prefixTrace trace (PPair a b) =
  PPair (prefixTrace trace a) (prefixTrace trace b)
prefixTrace trace (PNat n t) = PNat n (seqTrace trace t)
prefixTrace trace (PBool b t) = PBool b (seqTrace trace t)
prefixTrace trace (PChar c t) = PChar c (seqTrace trace t)
prefixTrace trace (PString s t) = PString s (seqTrace trace t)
prefixTrace trace (PFormat f t) = PFormat f (seqTrace trace t)
prefixTrace trace (PResult r t) = PResult r (seqTrace trace t)
prefixTrace trace (PNatList ns t) = PNatList ns (seqTrace trace t)

newtype TracedLawful a b = TracedLawful
  { runTracedLawful :: HasProvenance a -> HasProvenance b }

runWithLawful :: TracedLawful a b -> HasProvenance a -> (b, Trace)
runWithLawful arrow input =
  let output = runTracedLawful arrow input
  in (valueOf output, traceOf output)

instance Category TracedLawful where
  id = TracedLawful id
  TracedLawful g . TracedLawful f = TracedLawful (g . f)

instance Cartesian TracedLawful where
  exl = TracedLawful $ \(PPair a _) -> a
  exr = TracedLawful $ \(PPair _ b) -> b
  terminal = TracedLawful $ \_ -> PUnit
  TracedLawful f &&& TracedLawful g =
    TracedLawful $ \a -> PPair (f a) (g a)

  nat n = TracedLawful $ \PUnit -> PNat n (Atom (DoNat n))
  eqNat = TracedLawful $ \(PPair (PNat a p) (PNat b q)) ->
    PBool (a == b) (seqTrace (parTrace p q) (Atom (DoEqNat a b)))
  predNat = TracedLawful $ \(PNat n trace) ->
    PNat (predecessor n) (seqTrace trace (Atom (DoPredNat n)))
  addNat = TracedLawful $ \(PPair (PNat a p) (PNat b q)) ->
    PNat (a + b) (seqTrace (parTrace p q) (Atom (DoAddNat a b)))
  stringEmpty = TracedLawful $ \(PString s trace) ->
    PBool (null s) (seqTrace trace (Atom (DoStringEmpty s)))
  headChar = TracedLawful $ \(PString s trace) ->
    PChar (head s) (seqTrace trace (Atom (DoHeadChar s)))
  tailString = TracedLawful $ \(PString s trace) ->
    PString (tail s) (seqTrace trace (Atom (DoTailString s)))
  consChar = TracedLawful $ \(PPair (PChar c p) (PString s q)) ->
    PString (c : s) (seqTrace (parTrace p q) (Atom (DoConsChar c s)))
  char c = TracedLawful $ \PUnit -> PChar c (Atom (DoChar c))
  emptyString = TracedLawful $ \PUnit -> PString "" (Atom DoEmptyString)
  bool b = TracedLawful $ \PUnit -> PBool b (Atom (DoBool b))
  eqChar = TracedLawful $ \(PPair (PChar a p) (PChar b q)) ->
    PBool (a == b) (seqTrace (parTrace p q) (Atom (DoEqChar a b)))
  formatValue format = TracedLawful $ \PUnit ->
    PFormat format (Atom (DoFormat format))
  success = TracedLawful $ \(PPair (PString name p) (PFormat format q)) ->
    PResult (Right (ParseResult name format))
      (seqTrace (parTrace p q) (Atom (DoSuccess name format)))
  failure reason = TracedLawful $ \_ ->
    PResult (Left reason) (Atom (DoFailure reason))
  emptyNatList = TracedLawful $ \PUnit -> PNatList [] (Atom DoEmptyNatList)
  natListEmpty = TracedLawful $ \(PNatList ns trace) ->
    PBool (null ns) (seqTrace trace (Atom (DoNatListEmpty ns)))
  headNat = TracedLawful $ \(PNatList ns trace) ->
    PNat (head ns) (seqTrace trace (Atom (DoHeadNat ns)))
  tailNat = TracedLawful $ \(PNatList ns trace) ->
    PNatList (tail ns) (seqTrace trace (Atom (DoTailNat ns)))
  consNat = TracedLawful $ \(PPair (PNat n p) (PNatList ns q)) ->
    PNatList (n : ns) (seqTrace (parTrace p q) (Atom (DoConsNat n ns)))
  leNat = TracedLawful $ \(PPair (PNat a p) (PNat b q)) ->
    PBool (a <= b) (seqTrace (parTrace p q) (Atom (DoLeNat a b)))

  annotate _ arrow = arrow
  branch (TracedLawful yes) (TracedLawful no) =
    TracedLawful $ \(PPair (PBool condition trace) argument) ->
      let result = (if condition then yes else no) argument
      in prefixTrace (seqTrace trace (Atom (DoIf condition))) result
