{-# LANGUAGE GADTs #-}

module AstFib (Traced (..), eval, fibAst, main) where

import Control.Monad (forM_, unless)
import qualified Main as M

-- A typed syntax tree. Bind shares a computed value with the continuation;
-- otherwise fibAst would re-evaluate PredNat each time it used its result.
data Traced a where
  Pure :: a -> Traced a
  ConstNat :: M.Nat -> Traced M.Nat
  EqNat :: Traced M.Nat -> Traced M.Nat -> Traced Bool
  PredNat :: Traced M.Nat -> Traced M.Nat
  AddNat :: Traced M.Nat -> Traced M.Nat -> Traced M.Nat
  Branch :: Traced Bool -> Traced a -> Traced a -> Traced a
  Bind :: Traced a -> (a -> Traced b) -> Traced b

instance Functor Traced where
  fmap f action = Bind action (Pure . f)

instance Applicative Traced where
  pure = Pure
  function <*> argument = Bind function $ \f ->
    Bind argument (Pure . f)

instance Monad Traced where
  (>>=) = Bind

-- The children of EqNat and AddNat describe independent computations. Par
-- records that structure; evaluation itself still runs left to right.
eval :: Traced a -> (a, M.Trace)
eval (Pure value) = (value, M.Empty)
eval (ConstNat value) = (value, M.Atom (M.DoNat value))
eval (EqNat left right) =
  let (a, leftTrace) = eval left
      (b, rightTrace) = eval right
  in (a == b, M.seqTrace (M.parTrace leftTrace rightTrace)
                          (M.Atom (M.DoEqNat a b)))
eval (PredNat child) =
  let (value, trace) = eval child
  in (M.predecessor value, M.seqTrace trace (M.Atom (M.DoPredNat value)))
eval (AddNat left right) =
  let (a, leftTrace) = eval left
      (b, rightTrace) = eval right
  in (a + b, M.seqTrace (M.parTrace leftTrace rightTrace)
                        (M.Atom (M.DoAddNat a b)))
eval (Branch condition yes no) =
  let (decision, conditionTrace) = eval condition
      (value, chosenTrace) = eval (if decision then yes else no)
  in (value, M.seqTrace conditionTrace
                        (M.seqTrace (M.Atom (M.DoIf decision)) chosenTrace))
eval (Bind first next) =
  let (value, firstTrace) = eval first
      (result, nextTrace) = eval (next value)
  in (result, M.seqTrace firstTrace nextTrace)

fibAst :: M.Nat -> Traced M.Nat
fibAst n = do
  zero <- ConstNat 0
  isZero <- EqNat (pure n) (pure zero)
  Branch (pure isZero)
    (ConstNat 0)
    (do
      one <- ConstNat 1
      isOne <- EqNat (pure n) (pure one)
      Branch (pure isOne)
        (ConstNat 1)
        (AddNat
          (do
            leftInput <- PredNat (pure n)
            fibAst leftInput)
          (do
            rightInput <- PredNat (pure n)
            secondRightInput <- PredNat (pure rightInput)
            fibAst secondRightInput)))

main :: IO ()
main = do
  forM_ [0 .. 8] $ \n -> do
    let actual = eval (fibAst n)
        expected = M.runTraced (M.fib :: M.Traced M.Nat M.Nat) n
    unless (actual == expected) $
      error ("AST and Cartesian Fibonacci differ at input " ++ show n ++
             "\nAST: " ++ show actual ++ "\nCartesian: " ++ show expected)
    putStrLn ("fib(" ++ show n ++ ") = " ++ show (fst actual))
  let branchResult = eval (Branch (Pure True) (ConstNat 1) (ConstNat 2))
      expectedBranch = (1, M.seqTrace (M.Atom (M.DoIf True))
                                     (M.Atom (M.DoNat 1)))
  unless (branchResult == expectedBranch) $
    error "Branch evaluated an unchosen alternative"
  putStrLn "AST values and full structured traces agree for inputs 0..8."
