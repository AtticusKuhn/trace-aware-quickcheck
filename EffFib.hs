{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module EffFib (main) where

import Control.Effect (Eff, Effect, interpret, lift, run, send)
import qualified Control.Effect.State.Strict as State
import Control.Monad (forM_, unless, when)
import Main (Event (..), Nat, Trace (..), fib, predecessor, runTraced,
             seqTrace)
import qualified Main as Categorical

-- The operations are requests for values. Their interpreter decides whether
-- to record a trace; fibEff itself has no tracing code.
data Traced :: Effect where
  ConstNat :: Nat -> Traced m Nat
  EqNat :: Traced m Nat -> Tracd m Nat -> Traced m Bool
  PredNat :: Traced m Nat -> Traced m Nat
  AddNat :: Traced m Nat -> Traced m Nat -> Traced m Nat
  Branch :: Traced m Bool -> Traced m a -> Traced m a -> Traced m a

fibEff :: Nat -> Eff '[TracedNat] Nat
fibEff n = do
  zero <- send (Nat 0)
  isZero <- send (EqNat n zero) >>= send . Branch
  if isZero then send (Nat 0) else do
    one <- send (Nat 1)
    isOne <- send (EqNat n one) >>= send . Branch
    if isOne then send (Nat 1) else do
      leftInput <- send (PredNat n)
      left <- fibEff leftInput
      rightInput <- send (PredNat n)
      secondRightInput <- send (PredNat rightInput)
      right <- fibEff secondRightInput
      send (AddNat left right)

-- A pure interpreter demonstrates that the same program computes Fibonacci
-- without trace collection.
runPlain :: Nat -> Nat
runPlain n = run $ interpret (\request -> pure $ case request of
  Nat value -> value
  EqNat a b -> a == b
  PredNat value -> predecessor value
  AddNat a b -> a + b
  Branch decision -> decision) (fibEff n)

runWithTrace :: Nat -> (Nat, Trace)
runWithTrace n =
  let (trace, result) = run $ State.runState Empty $
        interpret (\request -> case request of
          Nat value -> record (DoNat value) value
          EqNat a b -> record (DoEqNat a b) (a == b)
          PredNat value -> record (DoPredNat value) (predecessor value)
          AddNat a b -> record (DoAddNat a b) (a + b)
          Branch decision -> record (DoIf decision) decision) (lift (fibEff n))
  in (result, trace)
  where
    record event value = do
      State.modify @Trace (\trace -> seqTrace trace (Atom event))
      pure value

-- The Cartesian (&&&) creates Par nodes. Ordinary Eff bind sequences the
-- recursive calls, so compare after mapping Par to sequential composition.
linearize :: Trace -> Trace
linearize Empty = Empty
linearize (Atom event) = Atom event
linearize (Seq left right) = seqTrace (linearize left) (linearize right)
linearize (Par left right) = seqTrace (linearize left) (linearize right)

main :: IO ()
main = do
  forM_ [0 .. 7] $ \n -> do
    let plain = runPlain n
        (actual, trace) = runWithTrace n
        (expected, categoricalTrace) = runTraced (fib :: Categorical.Traced Nat Nat) n
    unless (plain == expected && actual == expected && trace == linearize categoricalTrace) $
      error ("Eff Fibonacci mismatch at input " ++ show n)
    when (n == 2) $
      unless (trace /= categoricalTrace) $
        error "Expected the Cartesian trace for fib(2) to retain a Par node"
    putStrLn ("fib(" ++ show n ++ ") = " ++ show actual ++
              ", events = " ++ show (eventCount trace))
  putStrLn "Eff values and linearized traces agree with Cartesian fib for inputs 0..7."
  where
    eventCount Empty = 0 :: Int
    eventCount (Atom _) = 1
    eventCount (Seq left right) = eventCount left + eventCount right
    eventCount (Par left right) = eventCount left + eventCount right
