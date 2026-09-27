-- Build and run with:
--   ghc -O1 -Wall -Werror -main-is Algorithm1Fib -o algorithm1-fib Algorithm1Fib.hs Main.hs
--   ./algorithm1-fib [iterations [seed]]
module Algorithm1Fib (main) where

import Data.Bits (shiftR, xor)
import Data.Word (Word64)
import Main (Nat, ProvenanceTrace, ProvenanceTraced, fib, runWithProvenance, sameCollapsedGraph)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Text.Read (readMaybe)

-- Fibonacci's recursive trace grows exponentially, so this experiment uses
-- a finite input range. The corpus itself is deliberately unbounded, as in
-- algorithm 1: every non-counterexample is appended, including duplicates.
maxInput :: Nat
maxInput = 10

type Rng = Word64
type Family = (ProvenanceTrace, [Nat])
type Corpus = [Family]

next :: Rng -> Rng
next seed = seed + 0x9e3779b97f4a7c15

mix :: Rng -> Rng
mix value = z2 `xor` (z2 `shiftR` 31)
  where
    z1 = (value `xor` (value `shiftR` 30)) * 0xbf58476d1ce4e5b9
    z2 = (z1 `xor` (z1 `shiftR` 27)) * 0x94d049bb133111eb

draw :: Int -> Rng -> (Int, Rng)
draw upper seed =
  let seed' = next seed
  in (fromIntegral (mix seed' `mod` fromIntegral upper), seed')

generate :: Rng -> (Nat, Rng)
generate seed =
  let (n, seed') = draw (fromIntegral maxInput + 1) seed
  in (fromIntegral n, seed')

mutate :: Nat -> Rng -> (Nat, Rng)
mutate n seed =
  let (choice, seed') = draw 6 seed
      value = toInteger n
      candidates = [value - 2, value - 1, value + 1,
                    value + 2, value * 2, value `div` 2]
      bounded = max 0 (min (toInteger maxInput) (candidates !! choice))
  in (fromInteger bounded, seed')

-- First select a family uniformly, then a stored sample uniformly within it.
-- This is the Balanced Families Heuristic over the current corpus.
chooseParent :: Corpus -> Rng -> (Nat, Rng)
chooseParent corpus seed =
  let (familyIndex, seed') = draw (length corpus) seed
      samples = snd (corpus !! familyIndex)
      (sampleIndex, seed'') = draw (length samples) seed'
  in (samples !! sampleIndex, seed'')

addSample :: Nat -> ProvenanceTrace -> Corpus -> Corpus
addSample n trace [] = [(trace, [n])]
addSample n trace ((representative, samples) : rest)
  | sameCollapsedGraph trace representative =
      (representative, n : samples) : rest
  | otherwise = (representative, samples) : addSample n trace rest

-- An independent iterative oracle makes this an actual test of fib.
referenceFib :: Nat -> Nat
referenceFib n = go n 0 1
  where
    go 0 a b = a
    go remaining a b = go (remaining - 1) b (a + b)

type Counterexample = (Nat, Nat, Nat)

testSample :: Corpus -> Nat -> Either Counterexample Corpus
testSample corpus n =
  let ((actual, _), trace) =
        runWithProvenance (fib :: ProvenanceTraced Nat Nat) n
      expected = referenceFib n
  in if actual /= expected
       then Left (n, actual, expected)
       else Right (addSample n trace corpus)

seedCorpus :: [Nat] -> Corpus -> (Maybe Counterexample, Corpus)
seedCorpus [] corpus = (Nothing, corpus)
seedCorpus (sample : rest) corpus =
  case testSample corpus sample of
    Left failure -> (Just failure, corpus)
    Right corpus' -> seedCorpus rest corpus'

runIterations :: Int -> Rng -> Corpus -> (Maybe Counterexample, Corpus)
runIterations 0 _ corpus = (Nothing, corpus)
runIterations remaining seed corpus =
  let (mode, seed') = draw 2 seed
      (sample, seed'') =
        if mode == 0 || null corpus
          then generate seed'
          else let (parent, parentSeed) = chooseParent corpus seed'
               in mutate parent parentSeed
  in case testSample corpus sample of
       Left failure -> (Just failure, corpus)
       Right corpus' -> runIterations (remaining - 1) seed'' corpus'

parseArgs :: [String] -> Maybe (Int, Rng)
parseArgs [] = Just (200, 1)
parseArgs [iterations] = do
  count <- readMaybe iterations
  if count >= 0 then Just (count, 1) else Nothing
parseArgs [iterations, seed] = do
  count <- readMaybe iterations
  chosenSeed <- readMaybe seed
  if count >= 0 then Just (count, chosenSeed) else Nothing
parseArgs _ = Nothing

main :: IO ()
main = do
  arguments <- getArgs
  (iterations, seed) <- case parseArgs arguments of
    Just settings -> pure settings
    Nothing -> fail "Usage: algorithm1-fib [nonnegative-iterations [word64-seed]]"
  let (seedFailure, initialCorpus) = seedCorpus [0, 1] []
      (failure, corpus) = case seedFailure of
        Just counterexample -> (Just counterexample, initialCorpus)
        Nothing -> runIterations iterations seed initialCorpus
  case failure of
    Nothing -> pure ()
    Just counterexample -> printFailure counterexample
  putStrLn ("Trace families discovered: " ++ show (length corpus))
  mapM_ printFamily (zip [1 :: Int ..] corpus)
  case failure of
    Nothing -> pure ()
    Just _ -> exitFailure

printFailure :: Counterexample -> IO ()
printFailure (n, actual, expected) =
  putStrLn ("Counterexample: fib(" ++ show n ++ ") = " ++ show actual
            ++ ", expected " ++ show expected)

printFamily :: (Int, Family) -> IO ()
printFamily (index, (_, samples)) =
  putStrLn ("Family " ++ show index ++ ": " ++ show (length samples)
            ++ " samples; representative input " ++ show (last samples))
