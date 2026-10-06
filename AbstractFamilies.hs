-- Build with:
--   ghc -O1 -Wall -Werror -main-is AbstractFamilies -o abstract-families AbstractFamilies.hs Main.hs
--   ./abstract-families
--
-- A deliberately path-insensitive abstract interpreter.  The only abstract
-- input is Top: every primitive condition may return either Boolean value.
-- The Cartesian instance extracts a finite trace grammar from the original
-- programs.  Its least fixed point contains every concrete collapsed graph.
module AbstractFamilies (main) where

import Control.Category (Category (..))
import Control.Exception (evaluate)
import Control.Monad.State.Strict (StateT, execStateT, get, modify')
import Control.Monad.IO.Class (liftIO)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Set as Set
import Main
  ( Cartesian (..), Identifier (..), Nat, ParseError, ParseResult
  , ProvenanceTraced, collapsedSummary, fib, parseCli, runWithProvenance
  , summaryEdges, summaryFirst, summaryLast, summaryNodes )
import Prelude hiding ((.), id)
import System.Mem.StableName (StableName, makeStableName, eqStableName)

data Program
  = Silent
  | Tick
  | Follow Program Program
  | Together Program Program
  | EitherWay Program Program
  | Named Identifier Program

newtype Approx a b = Approx Program

-- Values are erased. Every branch contributes both alternatives, so the
-- resulting grammar over-approximates every terminating concrete execution.
instance Category Approx where
  id = Approx Silent
  Approx q . Approx p = Approx (Follow p q)

instance Cartesian Approx where
  fixArrow f = result
    where result@(Approx _) = f result
  exl = Approx Silent
  exr = Approx Silent
  terminal = Approx Silent
  Approx p &&& Approx q = Approx (Together p q)
  nat _ = Approx Tick
  eqNat = Approx Tick
  predNat = Approx Tick
  addNat = Approx Tick
  stringEmpty = Approx Tick
  headChar = Approx Tick
  tailString = Approx Tick
  consChar = Approx Tick
  char _ = Approx Tick
  emptyString = Approx Tick
  bool _ = Approx Tick
  eqChar = Approx Tick
  formatValue _ = Approx Tick
  success = Approx Tick
  failure _ = Approx Tick
  emptyNatList = Approx Tick
  natListEmpty = Approx Tick
  headNat = Approx Tick
  tailNat = Approx Tick
  consNat = Approx Tick
  leNat = Approx Tick
  annotate name (Approx p) = Approx (Named name p)
  branch (Approx yes) (Approx no) = Approx $
    EitherWay (Follow Tick yes) (Follow Tick no)

-- A graph summary retains boundaries for composition.  The final family key
-- forgets boundaries, just as Main.sameCollapsedGraph does.
data Summary = Summary
  { nodes :: Set.Set Identifier
  , first :: Set.Set Identifier
  , last_ :: Set.Set Identifier
  , edges :: Set.Set (Identifier, Identifier)
  } deriving (Eq, Ord)

empty :: Summary
empty = Summary Set.empty Set.empty Set.empty Set.empty

atom :: Identifier -> Summary
atom name = Summary one one one Set.empty
  where one = Set.singleton name

following :: Summary -> Summary -> Summary
following x y = Summary
  (Set.union (nodes x) (nodes y))
  (if Set.null (nodes x) then first y else first x)
  (if Set.null (nodes y) then last_ x else last_ y)
  (Set.unions [edges x, edges y, Set.fromList
    [(a,b) | a <- Set.toList (last_ x), b <- Set.toList (first y)]])

together :: Summary -> Summary -> Summary
together x y = Summary
  (Set.union (nodes x) (nodes y))
  (Set.union (first x) (first y))
  (Set.union (last_ x) (last_ y))
  (Set.union (edges x) (edges y))

type Family = (Set.Set Identifier, Set.Set (Identifier, Identifier))

family :: Summary -> Family
family value = (nodes value, edges value)

data Rule
  = REmpty | RAtom Identifier | RSeq Int Int | RPar Int Int
  | RChoice Int Int | RAlias Int
  deriving (Show)

data Builder = Builder
  { seen :: [(StableName Program, Maybe Identifier, Int)]
  , rules :: IntMap.IntMap Rule
  , nextId :: Int
  }

startBuilder :: Builder
startBuilder = Builder [] IntMap.empty 0

visit :: Maybe Identifier -> Program -> StateT Builder IO Int
visit context raw = do
  program <- liftIO' (evaluate raw)
  stable <- liftIO' (makeStableName program)
  state <- get
  case [i | (name, label, i) <- seen state,
            label == context && eqStableName name stable] of
    i : _ -> pure i
    [] -> do
      let i = nextId state
      if i > 10000 then fail "trace grammar exceeded 10000 rules; check fixArrow use"
        else pure ()
      modify' (\s -> s { seen = (stable, context, i) : seen s
                       , nextId = i + 1 })
      rule <- case program of
        Silent -> pure REmpty
        Tick -> case context of
          Just label -> pure (RAtom label)
          Nothing -> fail "unannotated trace event"
        Follow p q -> RSeq <$> visit context p <*> visit context q
        Together p q -> RPar <$> visit context p <*> visit context q
        EitherWay p q -> RChoice <$> visit context p <*> visit context q
        Named label p -> RAlias <$> visit (Just label) p
      modify' (\s -> s { rules = IntMap.insert i rule (rules s) })
      pure i

liftIO' :: IO a -> StateT Builder IO a
liftIO' = liftIO

evalRule :: IntMap.IntMap (Set.Set Summary) -> Rule -> Set.Set Summary
evalRule _ REmpty = Set.singleton empty
evalRule _ (RAtom name) = Set.singleton (atom name)
evalRule current (RAlias p) = IntMap.findWithDefault Set.empty p current
evalRule current (RChoice p q) = Set.union
  (IntMap.findWithDefault Set.empty p current)
  (IntMap.findWithDefault Set.empty q current)
evalRule current (RSeq p q) = Set.fromList
  [following x y | x <- Set.toList (IntMap.findWithDefault Set.empty p current),
                   y <- Set.toList (IntMap.findWithDefault Set.empty q current)]
evalRule current (RPar p q) = Set.fromList
  [together x y | x <- Set.toList (lookupSet p), y <- Set.toList (lookupSet q)]
  where
    lookupSet i = IntMap.findWithDefault Set.empty i current

solve :: IntMap.IntMap Rule -> IntMap.IntMap (Set.Set Summary)
solve grammar = go IntMap.empty
  where
    -- The summary universe is finite: there are finitely many identifiers
    -- and each node or edge is either present or absent. The ascending
    -- Kleene sequence therefore reaches a fixed point.
    go current =
      let updated = IntMap.map (evalRule current) grammar
      in if updated == current then current else go updated

analyse :: Approx a b -> IO (Set.Set Family, Int)
analyse (Approx program) = do
  builder <- execStateT (visit Nothing program) startBuilder
  let values = IntMap.findWithDefault Set.empty 0 (solve (rules builder))
  pure (Set.map family values, IntMap.size (rules builder))

concreteFib :: Nat -> Family
concreteFib = family . concreteFibSummary

concreteFibSummary :: Nat -> Summary
concreteFibSummary n =
  let (_, trace) = runWithProvenance (fib :: ProvenanceTraced Nat Nat) n
      summary = collapsedSummary trace
  in Summary (summaryNodes summary) (summaryFirst summary)
       (summaryLast summary) (summaryEdges summary)

-- An executable induction certificate for the concrete recursive case.
-- fib(2) combines fib(1) and fib(0). For n >= 3, the left child is
-- eventually in the stable family, while the right child is fib(2) or stable.
recursiveFib :: Summary -> Summary -> Summary
recursiveFib left right = following (atom CheckZero) $
  following (atom ZeroBranch) $
  following (atom CheckOne) $
  following (atom OneBranch) $
  following (together
    (following (atom LeftPred) left)
    (following (following (atom RightFirstPred) (atom RightSecondPred)) right))
    (atom CombineResults)

fibStabilizes :: Bool
fibStabilizes =
  let s0 = concreteFibSummary 0
      s1 = concreteFibSummary 1
      s2 = concreteFibSummary 2
      s3 = concreteFibSummary 3
  in recursiveFib s1 s0 == s2
     && recursiveFib s2 s1 == s3
     && recursiveFib s3 s2 == s3
     && recursiveFib s3 s3 == s3

concreteCli :: String -> Family
concreteCli input =
  let (_, trace) = runWithProvenance
        (parseCli :: ProvenanceTraced String (Either ParseError ParseResult)) input
      summary = collapsedSummary trace
  in (summaryNodes summary, summaryEdges summary)

describeSpuriousFib :: Family -> String
describeSpuriousFib (names, links) =
  "recursive trace with only " ++ base ++
  "; nested recursive combination: " ++ show nested
  where
    base = if Set.member ReturnZero names then "the zero base case"
      else "the one base case"
    nested = Set.member (CombineResults, CombineResults) links

-- These inputs are certificates for the CLI lower bound: each has a distinct
-- graph, and every graph in the abstract result has one of these witnesses.
cliWitnesses :: [String]
cliWitnesses =
  ["", "f", "-", "f other", "f --format", "f --format TSV",
   "--format CSV f", "--format CSV", "--format", "--format H",
   "f --format H", "--format CSV --bad", "f --format TSV extra",
   "--format TSV f extra"]

main :: IO ()
main = do
  (fibFamilies, fibRules) <- analyse (fib :: Approx Nat Nat)
  let actualFib = Set.fromList (map concreteFib [0..8])
  if fibStabilizes && actualFib == Set.fromList (map concreteFib [0..3])
    then pure () else fail "Fibonacci fixed-point certificate failed"
  putStrLn $ "Fibonacci: " ++ show fibRules ++ " grammar rules, "
    ++ show (Set.size fibFamilies) ++ " proposed families, "
    ++ show (Set.size actualFib) ++ " actual families, "
    ++ show (Set.size (Set.difference fibFamilies actualFib)) ++ " spurious"
  mapM_ (putStrLn . ("  spurious: " ++) . describeSpuriousFib)
    (Set.toList (Set.difference fibFamilies actualFib))
  if Set.isSubsetOf actualFib fibFamilies then pure () else fail "unsound Fibonacci result"
  (cliFamilies, cliRules) <- analyse (parseCli :: Approx String (Either ParseError ParseResult))
  let sampleCli = Set.fromList (map concreteCli cliWitnesses)
  putStrLn $ "CLI: " ++ show cliRules ++ " grammar rules, "
    ++ show (Set.size cliFamilies) ++ " proposed families, "
    ++ show (Set.size sampleCli) ++ " witnessed families, "
    ++ show (Set.size (Set.difference cliFamilies sampleCli)) ++ " spurious"
  if sampleCli == cliFamilies && Set.size sampleCli == length cliWitnesses
    then pure () else fail "CLI witness certificate failed"
