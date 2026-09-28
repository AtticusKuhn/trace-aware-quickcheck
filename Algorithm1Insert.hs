-- Build and run with:
--   ghc -O1 -Wall -Werror -package random -main-is Algorithm1Insert -o algorithm1-insert Algorithm1Insert.hs Main.hs
--   ./algorithm1-insert [iterations [seed]]
--
-- The property is the motivating sorted-list example in Lampropoulos,
-- Hicks, and Pierce, "Coverage Guided, Property Based Testing" (OOPSLA 2019).
module Algorithm1Insert (main) where

import Control.Category ((>>>))
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Main (Cartesian (..), Identifier (..), Nat, ProvenanceTrace,
             ProvenanceTraced, runWithProvenance, sameCollapsedGraph,
             ifThenElse)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.Random (StdGen, mkStdGen, randomR)
import Text.Read (readMaybe)
import Prelude hiding ((.), id)

type Sample = (Nat, [Nat])
type Family = (ProvenanceTrace, Set.Set Sample)
-- Keys preserve discovery order; membership uses the collapsed graph.
type Corpus = Map.Map Int Family
type Rng = StdGen

-- The only list operations in the categorical program are primitives:
-- empty, null, head, tail, and cons. Ordering is a primitive on Nat.
isSorted :: Cartesian k => k [Nat] Bool
isSorted =
  ifThenElse (annotate CheckSortedEmpty natListEmpty)
    (annotate SortedBase (terminal >>> bool True)) $
    ifThenElse (tailNat >>> annotate CheckSortedSingleton natListEmpty)
      (annotate SortedBase (terminal >>> bool True)) $
      ifThenElse ((headNat &&& (tailNat >>> headNat))
                  >>> annotate CheckSortedOrder leNat)
        (tailNat >>> isSorted)
        (annotate SortedDescent (terminal >>> bool False))

insert :: Cartesian k => k Sample [Nat]
insert =
  ifThenElse (exr >>> annotate CheckInsertEmpty natListEmpty)
    ((exl &&& (terminal >>> emptyNatList))
      >>> annotate InsertAtEnd consNat) $
    ifThenElse ((exl &&& (exr >>> headNat))
                >>> annotate CheckInsertOrder leNat)
      ((exl &&& exr) >>> annotate InsertAtHead consNat)
      (((exr >>> headNat) &&&
        ((exl &&& (exr >>> tailNat)) >>> insert))
        >>> annotate BuildInsertedList consNat)

-- Trace the guard, the insertion, and the sortedness check on its result.
propInsertSorted :: Cartesian k => k Sample Bool
propInsertSorted = annotate CheckPropertyGuard $
  ifThenElse (exr >>> isSorted)
    ((insert >>> isSorted))
    (terminal >>> bool True)

draw :: Int -> Rng -> (Int, Rng)
draw upper = randomR (0, upper - 1)

-- Plain bounded list generation, with no sortedness knowledge.
generate :: Rng -> (Sample, Rng)
generate seed =
  let (lengthXs, seed1) = randomR (0 :: Int, 12) seed
      (x, seed2) = randomR (0 :: Int, 20) seed1
      (xs, seed3) = generateList lengthXs seed2
  in ((fromIntegral x, xs), seed3)
  where
    generateList :: Int -> Rng -> ([Nat], Rng)
    generateList 0 current = ([], current)
    generateList n current =
      let (value, next) = randomR (0 :: Int, 20) current
          (rest, final) = generateList (n - 1) next
      in (fromIntegral value : rest, final)

-- Generic type-preserving edits to one component of the sample.
-- No edit constructs or sorts a whole valid list.
mutate :: Sample -> Rng -> (Sample, Rng)
mutate (x, xs) seed =
  let (operation, seed1) = draw 6 seed
      (position, seed2) = randomR (0, length xs) seed1
      (drawnValue, seed3) = randomR (0 :: Int, 20) seed2
      (drawnDelta, seed4) = randomR (0 :: Int, 3) seed3
      value = fromIntegral drawnValue
      delta = fromIntegral drawnDelta
      before = take position xs
      after = drop position xs
      bump n = if operation == 4 then n + delta else n - min n delta
      result = case operation of
        0 -> (value, xs)
        1 -> (x, before ++ value : after)
        2 -> (x, if null after then xs else before ++ tail after)
        3 -> (x, if null after then xs else before ++ value : tail after)
        4 -> (bump x, xs)
        _ -> (x, if null after then xs else before ++ bump (head after) : tail after)
  in (result, seed4)

-- Balanced Families Heuristic: choose a family, then an input in that family.
chooseParent :: Corpus -> Rng -> (Sample, Rng)
chooseParent corpus seed =
  let (familyIndex, seed1) = draw (Map.size corpus) seed
      (_, (_, samples)) = Map.elemAt familyIndex corpus
      (sampleIndex, seed2) = draw (Set.size samples) seed1
  in (Set.elemAt sampleIndex samples, seed2)

addSample :: Sample -> ProvenanceTrace -> Corpus -> Corpus
addSample sample trace corpus =
  case Map.foldrWithKey findFamily Nothing corpus of
    Just key -> Map.adjust (\(representative, samples) ->
      (representative, Set.insert sample samples)) key corpus
    Nothing -> Map.insert (Map.size corpus) (trace, Set.singleton sample) corpus
  where
    findFamily key (representative, _) found
      | sameCollapsedGraph trace representative = Just key
      | otherwise = found

testSample :: Corpus -> Sample -> Either Sample Corpus
testSample corpus sample =
  let ((holds, _), trace) =
        runWithProvenance (propInsertSorted :: ProvenanceTraced Sample Bool) sample
  in if holds then Right (addSample sample trace corpus) else Left sample

seedCorpus :: [Sample] -> Corpus -> (Maybe Sample, Corpus)
seedCorpus [] corpus = (Nothing, corpus)
seedCorpus (sample : rest) corpus =
  case testSample corpus sample of
    Left counterexample -> (Just counterexample, corpus)
    Right corpus' -> seedCorpus rest corpus'

runIterations :: Int -> Rng -> Corpus -> (Maybe Sample, Corpus)
runIterations 0 _ corpus = (Nothing, corpus)
runIterations remaining seed corpus =
  let (mode, seed1) = draw 2 seed
      (sample, seed2) =
        if mode == 0 || Map.null corpus
          then generate seed1
          else let (parent, parentSeed) = chooseParent corpus seed1
               in mutate parent parentSeed
  in case testSample corpus sample of
       Left counterexample -> (Just counterexample, corpus)
       Right corpus' -> runIterations (remaining - 1) seed2 corpus'

parseArgs :: [String] -> Maybe (Int, Int)
parseArgs [] = Just (500, 1)
parseArgs [iterations] = do
  count <- readMaybe iterations
  if count >= 0 then Just (count, 1) else Nothing
parseArgs [iterations, seed] = do
  count <- readMaybe iterations
  chosenSeed <- readMaybe seed
  if count >= 0 then Just (count, chosenSeed) else Nothing
parseArgs _ = Nothing

initialSamples :: [Sample]
initialSamples =
  [(0, []), (0, [0]), (1, [0, 1]), (1, [0, 2]),
   (3, [0, 2]), (1, [1, 0])]

main :: IO ()
main = do
  arguments <- getArgs
  (iterations, seed) <- case parseArgs arguments of
    Just settings -> pure settings
    Nothing -> fail "Usage: algorithm1-insert [nonnegative-iterations [int-seed]]"
  let (seedFailure, initialCorpus) = seedCorpus initialSamples Map.empty
      (found, corpus) = case seedFailure of
        Just counterexample -> (Just counterexample, initialCorpus)
        Nothing -> runIterations iterations (mkStdGen seed) initialCorpus
  case found of
    Nothing -> putStrLn "No counterexample found."
    Just sample -> putStrLn ("Counterexample: " ++ show sample)
  putStrLn ("Trace families discovered: " ++ show (Map.size corpus))
  mapM_ printFamily (zip [1 :: Int ..] (Map.elems corpus))
  case found of
    Nothing -> pure ()
    Just _ -> exitFailure

printFamily :: (Int, Family) -> IO ()
printFamily (index, (_, samples)) = do
  let ordered = sortOn (\(x, xs) -> (length xs, x, xs)) (Set.toList samples)
      countAt place = length (filter (\sample -> placement sample == place) ordered)
  putStrLn ("Family " ++ show index ++ ": " ++ show (Set.size samples)
            ++ " distinct inputs; empty/front/middle/end/vacuous = "
            ++ show (map countAt ["empty", "front", "middle", "end", "vacuous"]))
  let representatives =
        [sample | place <- ["empty", "front", "middle", "end", "vacuous"],
                  sample <- take 1 (filter (\candidate -> placement candidate == place) ordered)]
      examples = take 10 (representatives ++
                  filter (`notElem` representatives) ordered)
  mapM_ (\sample ->
    putStrLn ("  " ++ show sample ++ " -> " ++
      placement sample ++ ", " ++
      show ((propInsertSorted :: Sample -> Bool) sample))) examples

-- insert stops at the first element >= x. This is used only for reporting;
-- the categorical property above supplies the trace and verdict.
placement :: Sample -> String
placement (x, xs)
  | not ((isSorted :: [Nat] -> Bool) xs) = "vacuous"
  | null xs = "empty"
  | position == 0 = "front"
  | position == length xs = "end"
  | otherwise = "middle"
  where
    position = length (takeWhile (< x) xs)
