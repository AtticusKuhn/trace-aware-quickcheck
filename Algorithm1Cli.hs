-- Build and run with:
--   ghc -O1 -Wall -Werror -package random -main-is Algorithm1Cli -o algorithm1-cli Algorithm1Cli.hs Main.hs
--   ./algorithm1-cli [iterations [seed]]
module Algorithm1Cli (main) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List (sortOn)
import Data.Word (Word64)
import Main (ParseError, ParseResult, ProvenanceTrace, ProvenanceTraced,
             parseCli, runWithProvenance, sameCollapsedGraph)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.Random (StdGen, mkStdGen, randomR)
import Text.Read (readMaybe)

type Rng = StdGen
type Family = (ProvenanceTrace, Set.Set String)
-- Keys preserve discovery order; membership uses the collapsed graph.
type Corpus = Map.Map Int Family
type Counterexample = (String, Either ParseError ParseResult,
                       Either ParseError ParseResult)

draw :: Int -> Rng -> (Int, Rng)
draw upper = randomR (0, upper - 1)

pick :: [a] -> Rng -> (a, Rng)
pick options seed =
  let (index, seed') = draw (length options) seed
  in (options !! index, seed')

-- A small black-box string generator; it has no knowledge of the parser.
generate :: Rng -> (String, Rng)
generate seed =
  let (size, seed') = randomR (1 :: Int, 18) seed
  in go size seed'
  where
    alphabet = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789- \t"
    go 0 current = ("", current)
    go remaining current =
      let (c, next) = pick alphabet current
          (rest, final) = go (remaining - 1) next
      in (c : rest, final)

-- Local edits plus a few CLI-specific token edits. These do not construct a
-- complete valid command; feedback decides which resulting inputs to retain.
mutate :: String -> Rng -> (String, Rng)
mutate input seed =
  let (operation, seed') = draw 6 seed
      (position, seed'') = randomR (0, length input) seed'
      (character, seed''') = pick "abxy0- \t" seed''
      (token, seed'''') = pick ["--format", "CSV", "TSV", "JSON", "file", "--bogus"] seed'''
      before = take position input
      after = drop position input
      changed = case operation of
        0 -> before ++ character : after
        1 -> if null after then input else before ++ tail after
        2 -> if null after then input else before ++ character : tail after
        3 -> before ++ token ++ after
        4 -> input ++ " " ++ token
        _ -> token ++ " " ++ input
  in (changed, seed'''')

-- Balanced Families Heuristic: draw a family, then one of its samples.
chooseParent :: Corpus -> Rng -> (String, Rng)
chooseParent corpus seed =
  let (familyIndex, seed') = draw (Map.size corpus) seed
      (_, (_, samples)) = Map.elemAt familyIndex corpus
      (sampleIndex, seed'') = draw (Set.size samples) seed'
  in (Set.elemAt sampleIndex samples, seed'')

addSample :: String -> ProvenanceTrace -> Corpus -> Corpus
addSample input trace corpus =
  case Map.foldrWithKey findFamily Nothing corpus of
    Just key -> Map.adjust (\(representative, samples) ->
      (representative, Set.insert input samples)) key corpus
    Nothing -> Map.insert (Map.size corpus) (trace, Set.singleton input) corpus
  where
    findFamily key (representative, _) found
      | sameCollapsedGraph trace representative = Just key
      | otherwise = found

testSample :: Corpus -> String -> Either Counterexample Corpus
testSample corpus input =
  let ((actual, _), trace) =
        runWithProvenance
          (parseCli :: ProvenanceTraced String (Either ParseError ParseResult)) input
      expected = (parseCli :: String -> Either ParseError ParseResult) input
  in if actual /= expected
       then Left (input, actual, expected)
       else Right (addSample input trace corpus)

seedCorpus :: [String] -> Corpus -> (Maybe Counterexample, Corpus)
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
        if mode == 0 || Map.null corpus
          then generate seed'
          else let (parent, parentSeed) = chooseParent corpus seed'
               in mutate parent parentSeed
  in case testSample corpus sample of
       Left failure -> (Just failure, corpus)
       Right corpus' -> runIterations (remaining - 1) seed'' corpus'

parseArgs :: [String] -> Maybe (Int, Word64)
parseArgs [] = Just (500, 1)
parseArgs [iterations] = do
  count <- readMaybe iterations
  if count >= 0 then Just (count, 1) else Nothing
parseArgs [iterations, seed] = do
  count <- readMaybe iterations
  chosenSeed <- readMaybe seed
  if count >= 0 then Just (count, chosenSeed) else Nothing
parseArgs _ = Nothing

-- Seed several parser outcomes, then let the search discover other families.
initialSamples :: [String]
initialSamples =
  [ "file", "file extra", "--bogus file", "file --format CSV"
  , "file --format TSV", "file --format JSON", "--format CSV file"
  , "--format TSV file", "--format JSON file", "file --format"
  , "--format file", "file --format bad", "--format CSV"
  ]

main :: IO ()
main = do
  arguments <- getArgs
  (iterations, seed) <- case parseArgs arguments of
    Just settings -> pure settings
    Nothing -> fail "Usage: algorithm1-cli [nonnegative-iterations [word64-seed]]"
  let (seedFailure, initialCorpus) = seedCorpus initialSamples Map.empty
      (failure, corpus) = case seedFailure of
        Just counterexample -> (Just counterexample, initialCorpus)
        Nothing -> runIterations iterations (mkStdGen (fromIntegral seed)) initialCorpus
  case failure of
    Nothing -> pure ()
    Just (input, actual, expected) ->
      putStrLn ("Counterexample: " ++ show input ++ " -> " ++ show actual
                ++ ", expected " ++ show expected)
  putStrLn ("Trace families discovered: " ++ show (Map.size corpus))
  mapM_ printFamily (zip [1 :: Int ..] (Map.elems corpus))
  case failure of
    Nothing -> pure ()
    Just _ -> exitFailure

printFamily :: (Int, Family) -> IO ()
printFamily (index, (_, samples)) = do
  putStrLn ("Family " ++ show index ++ ": " ++ show (Set.size samples)
            ++ " distinct inputs")
  let examples = take 3 (sortOn (\input -> (length input, input))
                         (Set.toList samples))
  mapM_ (\input ->
    putStrLn ("  " ++ show input ++ " -> " ++
      show ((parseCli :: String -> Either ParseError ParseResult) input))) examples
