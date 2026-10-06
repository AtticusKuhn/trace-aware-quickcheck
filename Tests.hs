{-# LANGUAGE GADTs #-}

module Tests (main) where

import Control.Category ((>>>))
import qualified Control.Category as Cat
import Control.Monad (forM_, unless)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Main
  ( Cartesian (..)
  , Event (..)
  , Trace (..)
  , Identifier (..)
  , IdentifiedEvent (..)
  , Nat
  , ProvenanceEvent (ProvenanceEmptyString)
  , ProvenanceTrace (..)
  , ProvenanceTraced
  , ParseError
  , ParseResult
  , buildProvenanceGraph
  , constant
  , countTrace
  , fib
  , graphEvents
  , graphLinks
  , parseCli
  , runWithProvenance
  , sameCollapsedGraph
  )
import TracedLawful
  ( HasProvenance (..), TracedLawful, runTracedLawful, runWithLawful )

check :: String -> Bool -> IO ()
check description condition = unless condition (fail description)

atom :: Identifier -> ProvenanceTrace
atom name = ProvenanceAtom (IdentifiedEvent (Just name) ProvenanceEmptyString)

-- Independent reference: first build the full graph of individual executions,
-- then quotient its nodes and edges by identifier.
referenceGraph :: ProvenanceTrace -> (Set.Set Identifier, Set.Set (Identifier, Identifier))
referenceGraph trace = (nodes, edges)
  where
    (_, graph) = buildProvenanceGraph 0 trace
    names = Map.fromList
      [(node, name) | (node, event) <- graphEvents graph, Just name <- [identifier event]]
    nodes = Set.fromList (Map.elems names)
    edges = Set.fromList
      [(names Map.! a, names Map.! b) | (a, b) <- graphLinks graph]

main :: IO ()
main = do
  let a = atom CheckZero
      b = atom CheckOne
      c = atom CombineResults
      small =
        [ ProvenanceEmpty
        , a
        , b
        , ProvenanceSeq a b
        , ProvenanceSeq a (ProvenancePar b c)
        , ProvenanceSeq (ProvenancePar a b) c
        , ProvenanceSeq a a
        , ProvenanceSeq (ProvenanceSeq a b) c
        , ProvenanceSeq a (ProvenanceSeq b c)
        , ProvenancePar (ProvenanceSeq a b) (ProvenanceSeq a b)
        , ProvenanceSeq ProvenanceEmpty a
        , ProvenanceSeq a ProvenanceEmpty
        ]
  forM_ (zip [0 :: Int ..] small) $ \(i, p) ->
    forM_ (zip [0 :: Int ..] small) $ \(j, q) ->
      check ("small traces " ++ show (i, j)) $
        sameCollapsedGraph p q == (referenceGraph p == referenceGraph q)

  let fibTraces =
        [snd (runWithProvenance (fib :: ProvenanceTraced Nat Nat) n) | n <- [0 .. 10]]
      fibGraphs = map referenceGraph fibTraces
  forM_ (zip3 [0 :: Int ..] fibTraces fibGraphs) $ \(i, p, pGraph) ->
    forM_ (zip3 [0 :: Int ..] fibTraces fibGraphs) $ \(j, q, qGraph) ->
      check ("fib traces " ++ show (i, j)) $
        sameCollapsedGraph p q == (pGraph == qGraph)

  let families =
        [ [n | (n, trace) <- zip [0 :: Int ..] fibTraces,
               sameCollapsedGraph representative trace]
        | representative <- fibTraces ]
  check "Fibonacci collapsed graph families" $
    families == [[0], [1], [2]] ++ replicate 8 [3 .. 10]
  putStrLn "Fibonacci collapsed graph families (0..10): {0}, {1}, {2}, {3..10}"

  let cliTraces =
        [snd (runWithProvenance
          (parseCli :: ProvenanceTraced String (Either ParseError ParseResult)) input)
        | input <- ["", "f", "f --format JSON", "f --format CSV", "--format CSV f"]]
      cliGraphs = map referenceGraph cliTraces
  forM_ (zip3 [0 :: Int ..] cliTraces cliGraphs) $ \(i, p, pGraph) ->
    forM_ (zip3 [0 :: Int ..] cliTraces cliGraphs) $ \(j, q, qGraph) ->
      check ("CLI traces " ++ show (i, j)) $
        sameCollapsedGraph p q == (pGraph == qGraph)

  let input = PNat 7 (Atom (DoNat 99))
      f = predNat :: TracedLawful Nat Nat
      g = constant 3 :: TracedLawful Nat Nat
  check "lawful first projection" $
    runWithLawful ((f &&& g) >>> exl) input == runWithLawful f input
  check "lawful second projection" $
    runWithLawful ((f &&& g) >>> exr) input == runWithLawful g input
  check "lawful terminal discards prior provenance" $
    runWithLawful (f >>> terminal :: TracedLawful Nat ()) input == ((), Empty)
  check "lawful terminal discards input provenance" $
    runWithLawful (terminal :: TracedLawful Nat ()) input == ((), Empty)

  let productInput = PPair input (PBool True (Atom (DoIf True)))
  check "lawful product reconstruction" $
    runWithLawful ((exl &&& exr) :: TracedLawful (Nat, Bool) (Nat, Bool)) productInput
      == runWithLawful (Cat.id :: TracedLawful (Nat, Bool) (Nat, Bool)) productInput
  check "lawful pairing is natural" $
    runWithLawful (f >>> (f &&& g)) input
      == runWithLawful ((f >>> f) &&& (f >>> g)) input
  check "lawful projection retains only the selected trace" $
    case runTracedLawful (exl :: TracedLawful (Nat, Bool) Nat) productInput of
      PNat _ trace -> trace == Atom (DoNat 99)

  forM_ [0 .. 8] $ \n ->
    check ("lawful Fibonacci value " ++ show n) $
      fst (runWithLawful (fib :: TracedLawful Nat Nat) (PNat n Empty))
        == (fib :: Nat -> Nat) n
  let (_, zeroTrace) =
        runWithLawful (fib :: TracedLawful Nat Nat) (PNat 0 Empty)
  check "lawful branch retains its condition after a constant result" $
    countTrace (\event -> case event of
      DoEqNat 0 0 -> True
      _ -> False) zeroTrace > 0

  let cliInputs =
        ["", "f", "f --format JSON", "f --format CSV", "--format CSV f",
         "f --format", "--format TSV", "f other"]
  forM_ cliInputs $ \cliInput ->
    check ("lawful CLI value " ++ show cliInput) $
      fst (runWithLawful
        (parseCli :: TracedLawful String (Either ParseError ParseResult))
        (PString cliInput Empty)) == (parseCli :: String -> Either ParseError ParseResult) cliInput
