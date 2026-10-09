{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}

-- Build with:
--   ghc -O1 -Wall -Werror -main-is AbstractFamiliesConstraints \
--     -o abstract-families-constraints AbstractFamiliesConstraints.hs AbstractFamilies.hs Main.hs
--   ./abstract-families-constraints [random-seed] [--verbose]
--
-- The branch choices below describe abstract executions. A constraint is
-- recorded when a branch is taken; it is deliberately never checked.
module AbstractFamiliesConstraints (main) where

import AbstractFamilies (Approx, Family, analyse, fibStabilizes)
import Control.Category (Category (..))
import Control.Monad (forM_, unless)
import Data.List (foldl', intercalate)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Main
  ( Cartesian (..), Expr (..), Identifier, Nat, ParseError, ParseResult
  , ProvenanceEvent (..), ProvenanceTrace (..), ProvenanceTraced
  , IdentifiedEvent (..), collapsedSummary, fib, parseCli, runWithProvenance
  , summaryNodes, summaryEdges )
import Prelude hiding ((.), id)
import System.Environment (getArgs)
import System.Random (StdGen, mkStdGen, randomR)
import Text.Read (readMaybe)

-- Every atom retains a typed Expr, even though different atoms may have
-- different result types. The Boolean outcome is present only for DoIf.
data SymbolicEvent
  = forall a. Operation (Maybe Identifier) String (Expr a)
  | Decision (Maybe Identifier) (Expr Bool) Bool

instance Show SymbolicEvent where
  show (Operation name operation expression) =
    showName name ++ ":" ++ operation ++ "(" ++ show expression ++ ")"
  show (Decision name expression chosen) =
    showName name ++ ":If " ++ show chosen ++ "(" ++ show expression ++ ")"

showName :: Maybe Identifier -> String
showName Nothing = "<unannotated>"
showName (Just name) = show name

data SymbolicTrace
  = SEmpty
  | SAtom SymbolicEvent
  | SSeq SymbolicTrace SymbolicTrace
  | SPar SymbolicTrace SymbolicTrace

instance Show SymbolicTrace where
  show SEmpty = "[]"
  show (SAtom symbolicEvent) = show symbolicEvent
  show (SSeq left right) = "[" ++ show left ++ " ; " ++ show right ++ "]"
  show (SPar left right) = "(" ++ show left ++ " | " ++ show right ++ ")"

seqTrace :: SymbolicTrace -> SymbolicTrace -> SymbolicTrace
seqTrace SEmpty right = right
seqTrace left SEmpty = left
seqTrace left right = SSeq left right

parTrace :: SymbolicTrace -> SymbolicTrace -> SymbolicTrace
parTrace SEmpty right = right
parTrace left SEmpty = left
parTrace left right = SPar left right

labelTrace :: Identifier -> SymbolicTrace -> SymbolicTrace
labelTrace _ SEmpty = SEmpty
labelTrace name (SAtom (Operation current operation expression)) =
  SAtom (Operation (Just (maybe name id current)) operation expression)
labelTrace name (SAtom (Decision current expression outcome)) =
  SAtom (Decision (Just (maybe name id current)) expression outcome)
labelTrace name (SSeq left right) =
  SSeq (labelTrace name left) (labelTrace name right)
labelTrace name (SPar left right) =
  SPar (labelTrace name left) (labelTrace name right)

-- A symbolic arrow consumes an externally supplied sequence of branch
-- outcomes. Fuel bounds recursive arrows, including impossible abstract paths.
newtype Symbolic a b = Symbolic
  { runSymbolic :: Int -> [Bool] -> Expr a -> Maybe (Expr b, SymbolicTrace, [Bool]) }

instance Category Symbolic where
  id = Symbolic $ \_ choices expression -> Just (expression, SEmpty, choices)
  Symbolic g . Symbolic f = Symbolic $ \fuel choices expression -> do
    (middle, firstTrace, rest) <- f fuel choices expression
    (result, secondTrace, remaining) <- g fuel rest middle
    pure (result, seqTrace firstTrace secondTrace, remaining)

firstExpr :: Expr (a, b) -> Expr a
firstExpr (Pair left _) = left
firstExpr expression = First expression

secondExpr :: Expr (a, b) -> Expr b
secondExpr (Pair _ right) = right
secondExpr expression = Second expression

-- Annotation supplies the operation identity used in collapsed graphs.
-- A temporary identity is needed only until the enclosing annotate runs.
event :: String -> Expr a -> SymbolicTrace
event operation expression = SAtom (Operation Nothing operation expression)

primitive :: String -> (Expr a -> Expr b) -> Symbolic a b
primitive operation f = Symbolic $ \_ choices expression ->
  let result = f expression
  in Just (result, event operation result, choices)

instance Cartesian Symbolic where
  fixArrow f = self
    where
      self = Symbolic $ \fuel choices expression ->
        if fuel <= 0 then Nothing
        else runSymbolic (f self) (fuel - 1) choices expression
  exl = Symbolic $ \_ choices expression ->
    Just (firstExpr expression, SEmpty, choices)
  exr = Symbolic $ \_ choices expression ->
    Just (secondExpr expression, SEmpty, choices)
  terminal = Symbolic $ \_ choices _ -> Just (Unit, SEmpty, choices)
  Symbolic f &&& Symbolic g = Symbolic $ \fuel choices expression -> do
    (left, firstTrace, rest) <- f fuel choices expression
    (right, secondTrace, remaining) <- g fuel rest expression
    pure (Pair left right, parTrace firstTrace secondTrace, remaining)
  nat n = primitive "Nat" (const (Constant n))
  eqNat = primitive "EqNat" $ \expression ->
    Equal (firstExpr expression) (secondExpr expression)
  predNat = primitive "PredNat" Pred
  addNat = primitive "AddNat" $ \expression ->
    Add (firstExpr expression) (secondExpr expression)
  stringEmpty = primitive "StringEmpty" IsEmpty
  headChar = primitive "HeadChar" HeadChar
  tailString = primitive "TailString" TailString
  consChar = primitive "ConsChar" $ \expression ->
    ConsChar (firstExpr expression) (secondExpr expression)
  char c = primitive "Char" (const (LiteralChar c))
  emptyString = primitive "EmptyString" (const EmptyString)
  bool b = primitive "Bool" (const (LiteralBool b))
  eqChar = primitive "EqChar" $ \expression ->
    EqualChar (firstExpr expression) (secondExpr expression)
  formatValue value = primitive "Format" (const (LiteralFormat value))
  success = primitive "Success" $ \expression ->
    Parsed (firstExpr expression) (secondExpr expression)
  failure reason = primitive "Failure" (const (Failed reason))
  emptyNatList = primitive "EmptyNatList" (const EmptyNatList)
  natListEmpty = primitive "NatListEmpty" IsNatListEmpty
  headNat = primitive "HeadNat" HeadNat
  tailNat = primitive "TailNat" TailNat
  consNat = primitive "ConsNat" $ \expression ->
    ConsNat (firstExpr expression) (secondExpr expression)
  leNat = primitive "LeNat" $ \expression ->
    LessEqualNat (firstExpr expression) (secondExpr expression)
  annotate name (Symbolic run) = Symbolic $ \fuel choices expression -> do
    (result, trace, rest) <- run fuel choices expression
    pure (result, labelTrace name trace, rest)
  branch (Symbolic yes) (Symbolic no) = Symbolic $ \fuel choices expression ->
    case choices of
      [] -> Nothing
      selected : rest -> do
        let condition = firstExpr expression
            argument = secondExpr expression
            branchEvent = SAtom (Decision Nothing condition selected)
        (result, trace, remaining) <-
          (if selected then yes else no) fuel rest argument
        pure (result, seqTrace branchEvent trace, remaining)

constraints :: SymbolicTrace -> [(Expr Bool, Bool)]
constraints SEmpty = []
constraints (SAtom (Operation _ _ _)) = []
constraints (SAtom (Decision _ expression outcome)) = [(expression, outcome)]
constraints (SSeq left right) = constraints left ++ constraints right
constraints (SPar left right) = constraints left ++ constraints right

data Summary = Summary
  { nodes :: Set.Set Identifier
  , first :: Set.Set Identifier
  , last_ :: Set.Set Identifier
  , edges :: Set.Set (Identifier, Identifier) }

empty :: Summary
empty = Summary Set.empty Set.empty Set.empty Set.empty

summarize :: SymbolicTrace -> Summary
summarize SEmpty = empty
summarize (SAtom symbolicEvent) =
  let name = case symbolicEvent of
        Operation (Just value) _ _ -> value
        Decision (Just value) _ _ -> value
        _ -> error "unannotated symbolic event"
      one = Set.singleton name
  in Summary one one one Set.empty
summarize (SPar left right) =
  let x = summarize left; y = summarize right in Summary
    (Set.union (nodes x) (nodes y))
    (Set.union (first x) (first y))
    (Set.union (last_ x) (last_ y))
    (Set.union (edges x) (edges y))
summarize (SSeq left right) =
  let x = summarize left; y = summarize right in Summary
    (Set.union (nodes x) (nodes y))
    (if Set.null (nodes x) then first y else first x)
    (if Set.null (nodes y) then last_ x else last_ y)
    (Set.unions [edges x, edges y, Set.fromList
      [(a,b) | a <- Set.toList (last_ x), b <- Set.toList (first y)]])

traceFamily :: SymbolicTrace -> Family
traceFamily trace = let value = summarize trace in (nodes value, edges value)

replay :: Symbolic a b -> Int -> [Bool] -> Maybe SymbolicTrace
replay arrow fuel choices = do
  (_, trace, unused) <- runSymbolic arrow fuel choices Input
  if null unused then Just trace else Nothing

branchChoices :: ProvenanceTrace -> [Bool]
branchChoices ProvenanceEmpty = []
branchChoices (ProvenanceAtom identified) = case eventPayload identified of
  ProvenanceIf (choice, _) -> [choice]
  _ -> []
branchChoices (ProvenanceSeq left right) =
  branchChoices left ++ branchChoices right
branchChoices (ProvenancePar left right) =
  branchChoices left ++ branchChoices right

concreteConstraints :: ProvenanceTrace -> [(String, Bool)]
concreteConstraints ProvenanceEmpty = []
concreteConstraints (ProvenanceAtom identified) = case eventPayload identified of
  ProvenanceIf (choice, expression) -> [(show expression, choice)]
  _ -> []
concreteConstraints (ProvenanceSeq left right) =
  concreteConstraints left ++ concreteConstraints right
concreteConstraints (ProvenancePar left right) =
  concreteConstraints left ++ concreteConstraints right

-- Random finite trees are branch plans, not input values. In particular,
-- two recursive calls may independently choose incompatible base cases.
fibPlan :: Int -> StdGen -> ([Bool], StdGen)
fibPlan depth rng =
  let (choice, next) = randomR (0 :: Int, if depth == 0 then 1 else 2) rng
  in case choice of
    0 -> ([True], next)
    1 -> ([False, True], next)
    _ -> let (left, afterLeft) = fibPlan (depth - 1) next
             (right, afterRight) = fibPlan (depth - 1) afterLeft
         in ([False, False] ++ left ++ right, afterRight)

fibCandidates :: Int -> [SymbolicTrace]
fibCandidates seed = go 6000 (mkStdGen seed) []
  where
    go :: Int -> StdGen -> [SymbolicTrace] -> [SymbolicTrace]
    go 0 _ traces = traces
    go remaining rng traces =
      let (depth, next) = randomR (1 :: Int, 4) rng
          (plan, final) = fibPlan depth next
      in case replay (fib :: Symbolic Nat Nat) 6 plan of
        Nothing -> error "finite Fibonacci branch plan failed"
        Just trace -> go (remaining - 1) final (trace : traces)

cliWitnesses :: [String]
cliWitnesses =
  ["", "f", "-", "f other", "f --format", "f --format TSV",
   "--format CSV f", "--format CSV", "--format", "--format H",
   "f --format H", "--format CSV --bad", "f --format TSV extra",
   "--format TSV f extra"]

-- Prefix whitespace varies the scanning trace while retaining the seed's
-- parser path. The original witnesses guarantee at least one per family.
cliCandidates :: Int -> [SymbolicTrace]
cliCandidates seed = concatMap sample cliWitnesses
  where
    sample witness =
      [ symbolic input | input <- witness : variations witness ]
    variations witness = take 30 $ map (\n -> replicate n ' ' ++ witness) lengths
    lengths = randomStream (mkStdGen seed)
    randomStream rng =
      let (n, next) = randomR (0 :: Int, 4) rng
      in n : randomStream next
    symbolic input =
      let (_, concrete) = runWithProvenance
            (parseCli :: ProvenanceTraced String (Either ParseError ParseResult)) input
          choices = branchChoices concrete
          concreteSummary = collapsedSummary concrete
      in case replay
        (parseCli :: Symbolic String (Either ParseError ParseResult)) 200 choices of
          Nothing -> error "CLI symbolic replay failed"
          Just trace ->
            if traceFamily trace ==
              (summaryNodes concreteSummary, summaryEdges concreteSummary)
              && map (\(expression, outcome) -> (show expression, outcome))
                   (constraints trace) == concreteConstraints concrete
            then trace else error "CLI symbolic replay differs from concrete provenance"

-- Retain a random series of traces for every proposed family. Equal graphs
-- may have different trace trees and different constraint lists.
groupSamples :: Set.Set Family -> Int -> [SymbolicTrace]
             -> Map.Map Family [SymbolicTrace]
groupSamples proposed perFamily = foldl' add Map.empty
  where
    add groups trace =
      let key = traceFamily trace
          previous = Map.findWithDefault [] key groups
      in if Set.member key proposed && length previous < perFamily
         then Map.insert key (trace : previous) groups else groups

-- The full CLI traces carry long nested expressions for each character
-- scanned. Keep them in memory, but show only a small preview by default.
traceSize :: SymbolicTrace -> (Int, Int, Int)
traceSize SEmpty = (0, 0, 0)
traceSize (SAtom _) = (1, 0, 0)
traceSize (SSeq left right) =
  let (a, s, p) = traceSize left
      (b, t, q) = traceSize right
  in (a + b, s + t + 1, p + q)
traceSize (SPar left right) =
  let (a, s, p) = traceSize left
      (b, t, q) = traceSize right
  in (a + b, s + t, p + q + 1)

clip :: Int -> String -> String
clip limit value
  | length value <= limit = value
  | otherwise = take (limit - 3) value ++ "..."

showConstraint :: (Expr Bool, Bool) -> String
showConstraint (expression, outcome) =
  show expression ++ " = " ++ show outcome

previewConstraints :: [(Expr Bool, Bool)] -> String
previewConstraints entries =
  intercalate "; " (map (clip 58 . showConstraint) (take 4 entries))
    ++ if length entries > 4
       then "; ... (" ++ show (length entries - 4) ++ " more)"
       else ""

printFamilies :: Bool -> String -> Set.Set Family -> Set.Set Family
              -> Map.Map Family [SymbolicTrace] -> IO ()
printFamilies verbose title proposed witnessed samples = do
  putStrLn $ title ++ ": " ++ show (Set.size proposed) ++ " proposed families, "
    ++ show (Set.size witnessed) ++ " witnessed, "
    ++ show (Set.size (Set.difference proposed witnessed)) ++ " spurious"
  unless (Map.keysSet samples == proposed) $
    fail $ title ++ ": no sampled abstract trace for some proposed families"
  forM_ (zip [1 :: Int ..] (Set.toList proposed)) $ \(index, key) -> do
    let (names, links) = key
    putStrLn $ "Family " ++ show index
      ++ (if Set.member key witnessed then " (witnessed)" else " (spurious)")
      ++ ": nodes=" ++ show (Set.toList names)
      ++ ", edges=" ++ (if verbose then show (Set.toList links)
                       else show (Set.size links))
    forM_ (zip [1 :: Int ..] (reverse (samples Map.! key))) $ \(n, trace) -> do
      let (atoms, sequences, parallels) = traceSize trace
          conditions = constraints trace
      if verbose then do
        putStrLn $ "  sample " ++ show n ++ " trace: " ++ show trace
        putStrLn "    constraints:"
        forM_ conditions $ \condition ->
          putStrLn $ "      " ++ showConstraint condition
      else do
        putStrLn $ "  sample " ++ show n ++ ": " ++ show atoms ++ " events"
          ++ " (Seq " ++ show sequences ++ ", Par " ++ show parallels ++ ")"
          ++ ", " ++ show (length conditions) ++ " constraints"
        putStrLn $ "    " ++ previewConstraints conditions

main :: IO ()
main = do
  args <- getArgs
  let verbose = "--verbose" `elem` args
      seed = case filter (/= "--verbose") args of
        [] -> 20261009
        [value] -> case readMaybe value of
          Just number -> number
          Nothing -> error "random-seed must be an integer"
        _ -> error "usage: abstract-families-constraints [random-seed] [--verbose]"
  unless verbose $ putStrLn "Compact view; use --verbose for full traces and constraints."
  (fibFamilies, _) <- analyse (fib :: Approx Nat Nat)
  (cliFamilies, _) <- analyse
    (parseCli :: Approx String (Either ParseError ParseResult))
  let concreteFib n =
        let (_, trace) = runWithProvenance (fib :: ProvenanceTraced Nat Nat) n
            summary = collapsedSummary trace
        in (summaryNodes summary, summaryEdges summary)
      concreteCli input =
        let (_, trace) = runWithProvenance
              (parseCli :: ProvenanceTraced String (Either ParseError ParseResult)) input
            summary = collapsedSummary trace
        in (summaryNodes summary, summaryEdges summary)
      witnessedFib = Set.fromList (map concreteFib [0..3])
      witnessedCli = Set.fromList (map concreteCli cliWitnesses)
  unless (fibStabilizes && Set.isSubsetOf witnessedFib fibFamilies
          && witnessedCli == cliFamilies) $
    fail "concrete witness families differ from abstract proposals"
  printFamilies verbose "Fibonacci" fibFamilies witnessedFib
    (groupSamples fibFamilies 3 (fibCandidates seed))
  printFamilies verbose "CLI" cliFamilies witnessedCli
    (groupSamples cliFamilies 3 (cliCandidates seed))
