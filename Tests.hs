module Tests (main) where

import Control.Monad (forM_, unless)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Main
  ( Identifier (..)
  , IdentifiedEvent (..)
  , Nat
  , ProvenanceEvent (ProvenanceEmptyString)
  , ProvenanceTrace (..)
  , ProvenanceTraced
  , ParseError
  , ParseResult
  , buildProvenanceGraph
  , fib
  , graphEvents
  , graphLinks
  , parseCli
  , runWithProvenance
  , sameCollapsedGraph
  )

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
