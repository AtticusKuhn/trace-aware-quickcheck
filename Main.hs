{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Category (Category (..), (>>>))
import Control.Monad (forM_, unless)
import Data.List (intercalate, nub)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import Prelude hiding ((.), id)
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)
import System.FilePath ((</>))
import System.Process (callProcess)

type Nat = Natural

data Event
  = DoNat Nat
  | DoEqNat Nat Nat
  | DoPredNat Nat
  | DoAddNat Nat Nat
  | DoIf Bool
  | DoStringEmpty String
  | DoHeadChar String
  | DoTailString String
  | DoConsChar Char String
  | DoChar Char
  | DoEmptyString
  | DoBool Bool
  | DoEqChar Char Char
  | DoFormat Format
  | DoSuccess String Format
  | DoFailure ParseError
  | DoEmptyNatList
  | DoNatListEmpty [Nat]
  | DoHeadNat [Nat]
  | DoTailNat [Nat]
  | DoConsNat Nat [Nat]
  | DoLeNat Nat Nat
  deriving (Eq, Show)

data Trace = Empty | Atom Event | Seq Trace Trace | Par Trace Trace
  deriving (Eq)

countTrace :: (Event -> Bool) -> Trace -> Int
countTrace _ Empty = 0
countTrace f (Atom e) = if f e then 1 else 0
countTrace f (Seq t1 t2) = (countTrace f t1) + (countTrace f t2)
countTrace f (Par t1 t2) = (countTrace f t1) + (countTrace f t2)

-- These names identify expressions in fib, rather than individual executions.
data Identifier
  = ZeroBranch
  | CheckZero
  | ReturnZero
  | OneBranch
  | CheckOne
  | ReturnOne
  | LeftPred
  | RightFirstPred
  | RightSecondPred
  | CombineResults
  | SkipWhitespace
  | ReadToken
  | DropToken
  | CompareToken
  | CheckFirstArgument
  | CheckFormatFlag
  | CheckFormatValue
  | CheckTrailingInput
  | BuildResult
  | ReportError
  | CheckPropertyGuard | CheckSortedEmpty | CheckSortedSingleton
  | CheckSortedOrder | SortedBase | SortedDescent
  | CheckInsertEmpty | CheckInsertOrder | BuildInsertedList
  | InsertAtHead | InsertAtEnd
  deriving (Eq, Ord, Show)

instance Show Trace where
  show Empty = "[]"
  show (Atom event) = show event
  show trace@(Seq _ _) = "[" ++ intercalate " ; " (sequential trace) ++ "]"
    where
      sequential (Seq p q) = sequential p ++ sequential q
      sequential t = [show t]
  show trace@(Par _ _) = "(" ++ intercalate " | " (parallel trace) ++ ")"
    where
      parallel (Par p q) = parallel p ++ parallel q
      parallel t = [show t]

-- Cartesian law says that  (f &&& g) >>> exl = f, but in the traced category,
-- f => [Dof]
--  (f && g) >>> exl => (DoF | DoG)
-- same thing with the terminal law
---  f >>> terminal = terminal
-- but with traces
-- terminal => []
-- f >>> terminal => [DoF]
-- This divergence is intentional. Traced category does not obey Cartesian category equational laws.
class Category k => Cartesian k where
  exl :: k (a, b) a
  exr :: k (a, b) b
  terminal :: k a ()
  (&&&) :: k a b -> k a c -> k a (b, c)

  nat :: Nat -> k () Nat
  eqNat :: k (Nat, Nat) Bool
  predNat :: k Nat Nat
  addNat :: k (Nat, Nat) Nat
  stringEmpty :: k String Bool
  headChar :: k String Char
  tailString :: k String String
  consChar :: k (Char, String) String
  char :: Char -> k () Char
  emptyString :: k () String
  bool :: Bool -> k () Bool
  eqChar :: k (Char, Char) Bool
  formatValue :: Format -> k () Format
  success :: k (String, Format) (Either ParseError ParseResult)
  failure :: ParseError -> k a (Either ParseError ParseResult)
  emptyNatList :: k () [Nat]
  natListEmpty :: k [Nat] Bool
  headNat :: k [Nat] Nat
  tailNat :: k [Nat] [Nat]
  consNat :: k (Nat, [Nat]) [Nat]
  leNat :: k (Nat, Nat) Bool
  annotate :: Identifier -> k a b -> k a b

  -- Select an arrow before executing it, passing it the original input.
  -- This is extra control-flow structure, not supplied by products alone.
  branch :: k a b -> k a b -> k (Bool, a) b

  -- Bool * (A * A) => A; `if` itself is a Haskell keyword.
  -- This selects already-produced values, so it cannot undo their traces.
  if_ :: k (Bool, (a, a)) a
  if_ = branch exl exr

infixr 3 &&&

constant :: Cartesian k => Nat -> k a Nat
constant n = terminal >>> nat n

isNat :: Cartesian k => Nat -> k Nat Bool
isNat n = (id &&& constant n) >>> eqNat

ifThenElse :: Cartesian k => k a Bool -> k a b -> k a b -> k a b
ifThenElse condition yes no = (condition &&& id) >>> branch yes no

-- All computation here is expressed using categorical combinators and
-- primitives. Haskell's recursive binding ties the knot; recursion is an
-- additional facility, not a consequence of being a cartesian category.
fib :: Cartesian k => k Nat Nat
fib =
  annotate ZeroBranch $
    ifThenElse (annotate CheckZero (isNat 0))
      (annotate ReturnZero (constant 0))
      (annotate OneBranch $
        ifThenElse (annotate CheckOne (isNat 1))
          (annotate ReturnOne (constant 1))
          ( ((annotate LeftPred predNat >>> fib)
              &&&
              (annotate RightFirstPred predNat
                >>> annotate RightSecondPred predNat
                >>> fib))
            >>> annotate CombineResults addNat))

data Format = CSV | TSV | Json
  deriving (Eq, Show)

data ParseResult = ParseResult
  { fileName :: String
  , format :: Format
  } deriving (Eq, Show)

data ParseError
  = MissingFileName | MissingFormat | InvalidFormat | UnexpectedArgument
  deriving (Eq, Show)

constantChar :: Cartesian k => Char -> k a Char
constantChar c = terminal >>> char c

constantFormat :: Cartesian k => Format -> k a Format
constantFormat value = terminal >>> formatValue value

isChar :: Cartesian k => Char -> k Char Bool
isChar c = (id &&& constantChar c) >>> eqChar

-- Whitespace, token comparison, and scanning are built from single-character
-- operations, so their individual decisions remain visible in the trace.
isWhitespace :: Cartesian k => k Char Bool
isWhitespace = ifThenElse (isChar ' ') (constantBool True) $
  ifThenElse (isChar '\t') (constantBool True) $
    ifThenElse (isChar '\n') (constantBool True) (isChar '\r')

constantBool :: Cartesian k => Bool -> k a Bool
constantBool value = terminal >>> bool value

skipWhitespace :: Cartesian k => k String String
skipWhitespace = annotate SkipWhitespace $
  ifThenElse stringEmpty id $
    ifThenElse (headChar >>> isWhitespace)
      (tailString >>> skipWhitespace)
      id

readToken :: Cartesian k => k String String
readToken = annotate ReadToken $
  ifThenElse stringEmpty id $
    ifThenElse (headChar >>> isWhitespace)
      (constantStringEmpty)
      ((headChar &&& (tailString >>> readToken)) >>> consChar)

dropToken :: Cartesian k => k String String
dropToken = annotate DropToken $
  ifThenElse stringEmpty id $
    ifThenElse (headChar >>> isWhitespace) id (tailString >>> dropToken)

constantStringEmpty :: Cartesian k => k a String
constantStringEmpty = terminal >>> emptyString

matches :: Cartesian k => String -> k String Bool
matches [] = annotate CompareToken stringEmpty
matches (c : cs) = annotate CompareToken $
  ifThenElse stringEmpty (constantBool False) $
    ifThenElse (headChar >>> isChar c)
      (tailString >>> matches cs)
      (constantBool False)

startsWithDash :: Cartesian k => k String Bool
startsWithDash = ifThenElse stringEmpty (constantBool False) (headChar >>> isChar '-')

-- A CLI input is a whitespace-separated string. The single positional file
-- can appear before or after --format; a format value is case sensitive.
parseCli :: Cartesian k => k String (Either ParseError ParseResult)
parseCli = skipWhitespace >>> annotate CheckFirstArgument
  (ifThenElse stringEmpty (annotate ReportError (failure MissingFileName)) $
    ifThenElse (readToken >>> matches "--format")
      (dropToken >>> skipWhitespace >>> parseFormatFirst)
      (ifThenElse (readToken >>> startsWithDash)
        (annotate ReportError (failure UnexpectedArgument))
        ((readToken &&& (dropToken >>> skipWhitespace)) >>> parseFileFirst)))

parseFileFirst :: Cartesian k => k (String, String) (Either ParseError ParseResult)
parseFileFirst = annotate CheckFormatFlag $
  ifThenElse (exr >>> stringEmpty)
    (annotate BuildResult ((exl &&& constantFormat Json) >>> success)) $
    ifThenElse (exr >>> readToken >>> matches "--format")
      ((exl &&& (exr >>> dropToken >>> skipWhitespace)) >>> parseFormatAfterFile)
      (annotate ReportError (failure UnexpectedArgument))

parseFormatAfterFile :: Cartesian k => k (String, String) (Either ParseError ParseResult)
parseFormatAfterFile = annotate CheckFormatValue $
  ifThenElse (exr >>> stringEmpty) (annotate ReportError (failure MissingFormat)) $
    ifThenElse (exr >>> readToken >>> matches "CSV")
      (finishWithFormat CSV)
      (ifThenElse (exr >>> readToken >>> matches "TSV")
        (finishWithFormat TSV)
        (ifThenElse (exr >>> readToken >>> matches "JSON")
          (finishWithFormat Json)
          (annotate ReportError (failure InvalidFormat))))

finishWithFormat :: Cartesian k => Format -> k (String, String) (Either ParseError ParseResult)
finishWithFormat chosen = annotate CheckTrailingInput $
  ifThenElse (exr >>> dropToken >>> skipWhitespace >>> stringEmpty)
    (annotate BuildResult ((exl &&& constantFormat chosen) >>> success))
    (annotate ReportError (failure UnexpectedArgument))

parseFormatFirst :: Cartesian k => k String (Either ParseError ParseResult)
parseFormatFirst = annotate CheckFormatValue $
  ifThenElse stringEmpty (annotate ReportError (failure MissingFormat)) $
    ifThenElse (readToken >>> matches "CSV") (parseFileAfterFormat CSV) $
      ifThenElse (readToken >>> matches "TSV") (parseFileAfterFormat TSV) $
        ifThenElse (readToken >>> matches "JSON") (parseFileAfterFormat Json)
          (annotate ReportError (failure InvalidFormat))

parseFileAfterFormat :: Cartesian k => Format -> k String (Either ParseError ParseResult)
parseFileAfterFormat chosen = dropToken >>> skipWhitespace >>>
  (ifThenElse stringEmpty (annotate ReportError (failure MissingFileName)) $
    ifThenElse (readToken >>> startsWithDash)
      (annotate ReportError (failure UnexpectedArgument)) $
      annotate CheckTrailingInput
        (ifThenElse (dropToken >>> skipWhitespace >>> stringEmpty)
          (annotate BuildResult ((readToken &&& constantFormat chosen) >>> success))
          (annotate ReportError (failure UnexpectedArgument))))

-- Natural subtraction saturates at zero.
predecessor :: Nat -> Nat
predecessor 0 = 0
predecessor n = n - 1

instance Cartesian (->) where
  exl = fst
  exr = snd
  terminal _ = ()
  (f &&& g) a = (f a, g a)
  nat n () = n
  eqNat (a, b) = a == b
  predNat = predecessor
  addNat (a, b) = a + b
  stringEmpty = null
  headChar = head
  tailString = tail
  consChar (c, s) = c : s
  char c () = c
  emptyString () = ""
  bool b () = b
  eqChar (a, b) = a == b
  formatValue value () = value
  success (name, chosen) = Right (ParseResult name chosen)
  failure reason _ = Left reason
  emptyNatList () = []
  natListEmpty = null
  headNat = head
  tailNat = tail
  consNat (n, ns) = n : ns
  leNat (a, b) = a <= b
  annotate _ f = f
  branch yes no (condition, a) = if condition then yes a else no a

newtype Traced a b = Traced { runTraced :: a -> (b, Trace) }

-- Remove silent steps and right-associate sequential composition. Traces
-- produced by this interpreter therefore obey the sequential unit and
-- associativity laws even under structural equality.
seqTrace :: Trace -> Trace -> Trace
seqTrace Empty q = q
seqTrace p Empty = p
seqTrace (Seq p q) r = seqTrace p (seqTrace q r)
seqTrace p q = Seq p q

parTrace :: Trace -> Trace -> Trace
parTrace Empty q = q
parTrace p Empty = p
parTrace p q = Par p q

instance Category Traced where
  id = Traced (\a -> (a, Empty))
  Traced g . Traced f = Traced $ \a ->
    let (b, p) = f a
        (c, q) = g b
    in (c, seqTrace p q)

instance Cartesian Traced where
  exl = Traced (\(a, _) -> (a, Empty))
  exr = Traced (\(_, b) -> (b, Empty))
  terminal = Traced (\_ -> ((), Empty))
  Traced f &&& Traced g = Traced $ \a ->
    let (b, p) = f a
        (c, q) = g a
    in ((b, c), parTrace p q)
  nat n = Traced (\() -> (n, Atom (DoNat n)))
  eqNat = Traced (\(a, b) -> (a == b, Atom (DoEqNat a b)))
  predNat = Traced (\n -> (predecessor n, Atom (DoPredNat n)))
  addNat = Traced (\(a, b) -> (a + b, Atom (DoAddNat a b)))
  stringEmpty = Traced (\s -> (null s, Atom (DoStringEmpty s)))
  headChar = Traced (\s -> (head s, Atom (DoHeadChar s)))
  tailString = Traced (\s -> (tail s, Atom (DoTailString s)))
  consChar = Traced (\(c, s) -> (c : s, Atom (DoConsChar c s)))
  char c = Traced (\() -> (c, Atom (DoChar c)))
  emptyString = Traced (\() -> ("", Atom DoEmptyString))
  bool b = Traced (\() -> (b, Atom (DoBool b)))
  eqChar = Traced (\(a, b) -> (a == b, Atom (DoEqChar a b)))
  formatValue value = Traced (\() -> (value, Atom (DoFormat value)))
  success = Traced (\(name, chosen) ->
    (Right (ParseResult name chosen), Atom (DoSuccess name chosen)))
  failure reason = Traced (\_ -> (Left reason, Atom (DoFailure reason)))
  emptyNatList = Traced (\() -> ([], Atom DoEmptyNatList))
  natListEmpty = Traced (\xs -> (null xs, Atom (DoNatListEmpty xs)))
  headNat = Traced (\xs -> (head xs, Atom (DoHeadNat xs)))
  tailNat = Traced (\xs -> (tail xs, Atom (DoTailNat xs)))
  consNat = Traced (\(n, ns) -> (n : ns, Atom (DoConsNat n ns)))
  leNat = Traced (\(a, b) -> (a <= b, Atom (DoLeNat a b)))
  annotate _ f = f
  branch (Traced yes) (Traced no) = Traced $ \(condition, a) ->
    let (b, trace) = if condition then yes a else no a
    in (b, seqTrace (Atom (DoIf condition)) trace)

-- An expression records how a value was computed. Its type matches the
-- value it describes, including booleans and products used by the arrows.
data Expr a where
  Input :: Expr a
  Constant :: Nat -> Expr Nat
  Unit :: Expr ()
  Pair :: Expr a -> Expr b -> Expr (a, b)
  First :: Expr (a, b) -> Expr a
  Second :: Expr (a, b) -> Expr b
  Pred :: Expr Nat -> Expr Nat
  Add :: Expr Nat -> Expr Nat -> Expr Nat
  Equal :: Expr Nat -> Expr Nat -> Expr Bool
  EmptyString :: Expr String
  LiteralChar :: Char -> Expr Char
  LiteralBool :: Bool -> Expr Bool
  IsEmpty :: Expr String -> Expr Bool
  HeadChar :: Expr String -> Expr Char
  TailString :: Expr String -> Expr String
  ConsChar :: Expr Char -> Expr String -> Expr String
  EqualChar :: Expr Char -> Expr Char -> Expr Bool
  LiteralFormat :: Format -> Expr Format
  Parsed :: Expr String -> Expr Format -> Expr (Either ParseError ParseResult)
  Failed :: ParseError -> Expr (Either ParseError ParseResult)
  EmptyNatList :: Expr [Nat]
  IsNatListEmpty :: Expr [Nat] -> Expr Bool
  HeadNat :: Expr [Nat] -> Expr Nat
  TailNat :: Expr [Nat] -> Expr [Nat]
  ConsNat :: Expr Nat -> Expr [Nat] -> Expr [Nat]
  LessEqualNat :: Expr Nat -> Expr Nat -> Expr Bool

instance Show (Expr a) where
  show = prettyExpr 0

prettyExpr :: Int -> Expr a -> String
prettyExpr _ Input = "input"
prettyExpr _ (Constant n) = show n
prettyExpr _ Unit = "()"
prettyExpr _ (Pair a b) = "(" ++ prettyExpr 0 a ++ ", " ++ prettyExpr 0 b ++ ")"
prettyExpr _ (First a) = "fst(" ++ prettyExpr 0 a ++ ")"
prettyExpr _ (Second a) = "snd(" ++ prettyExpr 0 a ++ ")"
prettyExpr _ (Pred a) = "pred(" ++ prettyExpr 0 a ++ ")"
prettyExpr precedence (Add a b) =
  parenthesize (precedence > 6) (prettyExpr 6 a ++ "+" ++ prettyExpr 7 b)
prettyExpr precedence (Equal a b) =
  parenthesize (precedence > 4) (prettyExpr 5 a ++ "==" ++ prettyExpr 5 b)
prettyExpr _ EmptyString = "\"\""
prettyExpr _ (LiteralChar c) = show c
prettyExpr _ (LiteralBool b) = show b
prettyExpr _ (IsEmpty s) = "null(" ++ show s ++ ")"
prettyExpr _ (HeadChar s) = "head(" ++ show s ++ ")"
prettyExpr _ (TailString s) = "tail(" ++ show s ++ ")"
prettyExpr _ (ConsChar c s) = "cons(" ++ show c ++ ", " ++ show s ++ ")"
prettyExpr _ (EqualChar a b) = show a ++ "==" ++ show b
prettyExpr _ (LiteralFormat value) = show value
prettyExpr _ (Parsed name chosen) = "ParseResult(" ++ show name ++ ", " ++ show chosen ++ ")"
prettyExpr _ (Failed reason) = "Left " ++ show reason
prettyExpr _ EmptyNatList = "[]"
prettyExpr _ (IsNatListEmpty xs) = "null(" ++ show xs ++ ")"
prettyExpr _ (HeadNat xs) = "head(" ++ show xs ++ ")"
prettyExpr _ (TailNat xs) = "tail(" ++ show xs ++ ")"
prettyExpr _ (ConsNat n xs) = "cons(" ++ show n ++ ", " ++ show xs ++ ")"
prettyExpr _ (LessEqualNat a b) = show a ++ "<=" ++ show b

parenthesize :: Bool -> String -> String
parenthesize True s = "(" ++ s ++ ")"
parenthesize False s = s

type Provenanced a = (a, Expr a)

data ProvenanceEvent
  = ProvenanceNat (Provenanced Nat)
  | ProvenanceEqNat (Provenanced Nat) (Provenanced Nat)
  | ProvenancePredNat (Provenanced Nat)
  | ProvenanceAddNat (Provenanced Nat) (Provenanced Nat)
  | ProvenanceIf (Provenanced Bool)
  | ProvenanceStringEmpty (Provenanced String)
  | ProvenanceHeadChar (Provenanced String)
  | ProvenanceTailString (Provenanced String)
  | ProvenanceConsChar (Provenanced Char) (Provenanced String)
  | ProvenanceChar (Provenanced Char)
  | ProvenanceEmptyString
  | ProvenanceBool (Provenanced Bool)
  | ProvenanceEqChar (Provenanced Char) (Provenanced Char)
  | ProvenanceFormat (Provenanced Format)
  | ProvenanceSuccess (Provenanced String) (Provenanced Format)
  | ProvenanceFailure ParseError
  | ProvenanceEmptyNatList
  | ProvenanceNatListEmpty (Provenanced [Nat])
  | ProvenanceHeadNat (Provenanced [Nat])
  | ProvenanceTailNat (Provenanced [Nat])
  | ProvenanceConsNat (Provenanced Nat) (Provenanced [Nat])
  | ProvenanceLeNat (Provenanced Nat) (Provenanced Nat)
  deriving (Show)

data IdentifiedEvent = IdentifiedEvent
  { identifier :: Maybe Identifier
  , eventPayload :: ProvenanceEvent
  } deriving (Show)

data ProvenanceTrace
  = ProvenanceEmpty
  | ProvenanceAtom IdentifiedEvent
  | ProvenanceSeq ProvenanceTrace ProvenanceTrace
  | ProvenancePar ProvenanceTrace ProvenanceTrace

instance Show ProvenanceTrace where
  show ProvenanceEmpty = "[]"
  show (ProvenanceAtom event) = show event
  show trace@(ProvenanceSeq _ _) = "[" ++ intercalate " ; " (sequential trace) ++ "]"
    where
      sequential (ProvenanceSeq p q) = sequential p ++ sequential q
      sequential t = [show t]
  show trace@(ProvenancePar _ _) = "(" ++ intercalate " | " (parallel trace) ++ ")"
    where
      parallel (ProvenancePar p q) = parallel p ++ parallel q
      parallel t = [show t]

-- Entries and exits are the event nodes at the boundary of a trace. They
-- let sequential composition connect both sides of a fork without adding
-- artificial fork or join events to the graph.
data DotGraph = DotGraph
  { graphEntries :: [Int]
  , graphExits :: [Int]
  , graphNodes :: [String]
  , graphEdges :: [String]
  , graphEvents :: [(Int, IdentifiedEvent)]
  , graphLinks :: [(Int, Int)]
  }

buildProvenanceGraph :: Int -> ProvenanceTrace -> (Int, DotGraph)
buildProvenanceGraph next ProvenanceEmpty =
  (next, DotGraph [] [] [] [] [] [])
buildProvenanceGraph next (ProvenanceAtom event) =
  (next + 1, DotGraph [next] [next] [node] [] [(next, event)] [])
  where
    node = "  n" ++ show next ++ " [label=" ++ dotQuote (eventLabel (eventPayload event))
      ++ ", fillcolor=" ++ dotQuote (eventColor (eventPayload event)) ++ "];"
buildProvenanceGraph next (ProvenanceSeq left right) =
  (afterRight, DotGraph entries exits nodes edges events links)
  where
    (afterLeft, leftGraph) = buildProvenanceGraph next left
    (afterRight, rightGraph) = buildProvenanceGraph afterLeft right
    entries = if null (graphEntries leftGraph)
      then graphEntries rightGraph else graphEntries leftGraph
    exits = if null (graphExits rightGraph)
      then graphExits leftGraph else graphExits rightGraph
    nodes = graphNodes leftGraph ++ graphNodes rightGraph
    joins = ["  n" ++ show a ++ " -> n" ++ show b ++ ";"
            | a <- graphExits leftGraph, b <- graphEntries rightGraph]
    edges = graphEdges leftGraph ++ graphEdges rightGraph ++ joins
    events = graphEvents leftGraph ++ graphEvents rightGraph
    links = graphLinks leftGraph ++ graphLinks rightGraph ++
      [(a, b) | a <- graphExits leftGraph, b <- graphEntries rightGraph]
buildProvenanceGraph next (ProvenancePar left right) =
  (afterRight, DotGraph entries exits nodes edges events links)
  where
    (afterLeft, leftGraph) = buildProvenanceGraph next left
    (afterRight, rightGraph) = buildProvenanceGraph afterLeft right
    entries = graphEntries leftGraph ++ graphEntries rightGraph
    exits = graphExits leftGraph ++ graphExits rightGraph
    nodes = graphNodes leftGraph ++ graphNodes rightGraph
    edges = graphEdges leftGraph ++ graphEdges rightGraph
    events = graphEvents leftGraph ++ graphEvents rightGraph
    links = graphLinks leftGraph ++ graphLinks rightGraph

provenanceTraceToDot :: ProvenanceTrace -> String
provenanceTraceToDot trace = unlines $
  [ "digraph provenance {"
  , "  graph [rankdir=TB, bgcolor=white, nodesep=0.35, ranksep=0.55];"
  , "  node [shape=box, style=\"rounded,filled\", color=\"#607080\", fontname=Helvetica, fontsize=11, margin=\"0.12,0.08\"];"
  , "  edge [color=\"#738496\", arrowsize=0.65];"
  ] ++ graphNodes graph ++ graphEdges graph ++ ["}"]
  where
    (_, graph) = buildProvenanceGraph 0 trace

-- Keep every execution event and edge, but enclose events from the same
-- annotated source expression in a Graphviz cluster.
groupedProvenanceTraceToDot :: ProvenanceTrace -> String
groupedProvenanceTraceToDot trace = unlines $
  [ "digraph provenance_grouped {"
  , "  graph [rankdir=TB, bgcolor=white, nodesep=0.35, ranksep=0.7, pad=0.2];"
  , "  node [shape=box, style=\"rounded,filled\", color=\"#607080\", fontname=Helvetica, fontsize=11, margin=\"0.12,0.08\"];"
  , "  edge [color=\"#738496\", arrowsize=0.65];"
  ] ++ clusters ++ unlabelledNodes ++ graphEdges graph ++ ["}"]
  where
    (_, graph) = buildProvenanceGraph 0 trace
    nodeLines = Map.fromList
      [(nodeId, line) | ((nodeId, _), line) <- zip (graphEvents graph) (graphNodes graph)]
    identifiers = nub [name | (_, identified) <- graphEvents graph,
      Just name <- [identifier identified]]
    nodesFor name = [nodeId | (nodeId, identified) <- graphEvents graph,
      identifier identified == Just name]
    clusters = concatMap clusterFor identifiers
    clusterFor name =
      [ "  subgraph cluster_" ++ show name ++ " {"
      , "    label=" ++ dotQuote (show name ++ " (" ++ show count
          ++ if count == 1 then " event)" else " events)") ++ ";"
      , "    style=rounded;"
      , "    color=" ++ dotQuote (identifierBorderColor name) ++ ";"
      , "    fontname=Helvetica;"
      , "    fontsize=12;"
      , "    penwidth=1.2;"
      ] ++ map (nodeLines Map.!) members ++ ["  }"]
      where
        members = nodesFor name
        count = length members
    unlabelledNodes = [nodeLines Map.! nodeId | (nodeId, identified) <- graphEvents graph,
      identifier identified == Nothing]

data CollapseKey = Labelled Identifier | Unlabelled Int
  deriving (Eq, Ord)

-- A trace fragment only needs its boundary nodes to compose with another
-- fragment. All sets are keyed by identifier, so repeated executions add no
-- work to the final graph beyond visiting their atoms.
data CollapsedSummary = CollapsedSummary
  { summaryNodes :: Set.Set Identifier
  , summaryFirst :: Set.Set Identifier
  , summaryLast :: Set.Set Identifier
  , summaryEdges :: Set.Set (Identifier, Identifier)
  }

collapsedSummary :: ProvenanceTrace -> CollapsedSummary
collapsedSummary ProvenanceEmpty = CollapsedSummary Set.empty Set.empty Set.empty Set.empty
collapsedSummary (ProvenanceAtom event) =
  let name = case identifier event of
        Just value -> value
        Nothing -> error "collapsedSummary: every atom must have an Identifier"
      one = Set.singleton name
  in CollapsedSummary one one one Set.empty
collapsedSummary (ProvenancePar p q) =
  let x = collapsedSummary p
      y = collapsedSummary q
  in CollapsedSummary
      (Set.union (summaryNodes x) (summaryNodes y))
      (Set.union (summaryFirst x) (summaryFirst y))
      (Set.union (summaryLast x) (summaryLast y))
      (Set.union (summaryEdges x) (summaryEdges y))
collapsedSummary (ProvenanceSeq p q) =
  let x = collapsedSummary p
      y = collapsedSummary q
      joins = Set.fromList
        [(a, b) | a <- Set.toList (summaryLast x), b <- Set.toList (summaryFirst y)]
  in CollapsedSummary
      (Set.union (summaryNodes x) (summaryNodes y))
      (if Set.null (summaryNodes x) then summaryFirst y else summaryFirst x)
      (if Set.null (summaryNodes y) then summaryLast x else summaryLast y)
      (Set.unions [summaryEdges x, summaryEdges y, joins])

-- First and last are needed while folding, but the collapsed DOT graph has
-- no entry or exit markers. Its equality is exactly nodes plus edge presence.
sameCollapsedGraph :: ProvenanceTrace -> ProvenanceTrace -> Bool
sameCollapsedGraph p q =
  let x = collapsedSummary p
      y = collapsedSummary q
  in summaryNodes x == summaryNodes y && summaryEdges x == summaryEdges y

-- Quotient the execution graph by identifier. Unlabelled occurrences stay
-- separate; repeated links are represented by a single weighted edge.
collapsedProvenanceTraceToDot :: ProvenanceTrace -> String
collapsedProvenanceTraceToDot trace = unlines $
  [ "digraph provenance_collapsed {"
  , "  graph [rankdir=LR, bgcolor=white, nodesep=0.55, ranksep=0.85, pad=0.2];"
  , "  node [shape=box, style=\"rounded,filled\", color=\"#607080\", fontname=Helvetica, fontsize=11, margin=\"0.16,0.1\"];"
  , "  edge [color=\"#738496\", fontcolor=\"#526273\", fontname=Helvetica, fontsize=10, arrowsize=0.65];"
  ] ++ nodes ++ edges ++ ["}"]
  where
    (_, graph) = buildProvenanceGraph 0 trace
    eventsById = Map.fromList (graphEvents graph)
    keyFor nodeId = case identifier (eventsById Map.! nodeId) of
      Just name -> Labelled name
      Nothing -> Unlabelled nodeId
    keys = nub [keyFor nodeId | (nodeId, _) <- graphEvents graph]
    ids = Map.fromList (zip keys [0 :: Int ..])
    nodeName key = "c" ++ show (ids Map.! key)
    eventCounts = Map.fromListWith (+)
      [(keyFor nodeId, 1 :: Int) | (nodeId, _) <- graphEvents graph]
    nodeLabel (Labelled name) =
      show name ++ "\n" ++ show count ++ if count == 1 then " event" else " events"
      where count = eventCounts Map.! (Labelled name)
    nodeLabel (Unlabelled nodeId) =
      eventLabel (eventPayload (eventsById Map.! nodeId))
    nodeColor (Labelled name) = identifierColor name
    nodeColor (Unlabelled nodeId) =
      eventColor (eventPayload (eventsById Map.! nodeId))
    nodes = ["  " ++ nodeName key ++ " [label=" ++ dotQuote (nodeLabel key)
      ++ ", fillcolor=" ++ dotQuote (nodeColor key) ++ "];" | key <- keys]
    edgeCounts = Map.fromListWith (+)
      [((keyFor from, keyFor to), 1 :: Int) | (from, to) <- graphLinks graph]
    edges = ["  " ++ nodeName from ++ " -> " ++ nodeName to
      ++ (if count == 1 then "" else " [label=" ++ dotQuote ("×" ++ show count) ++ "]")
      ++ ";" | ((from, to), count) <- Map.toList edgeCounts]

identifierColor :: Identifier -> String
identifierColor ZeroBranch = "#fff0ed"
identifierColor CheckZero = "#e4f4f0"
identifierColor ReturnZero = "#eaf2ff"
identifierColor OneBranch = "#fff0ed"
identifierColor CheckOne = "#e4f4f0"
identifierColor ReturnOne = "#eaf2ff"
identifierColor LeftPred = "#fff0dc"
identifierColor RightFirstPred = "#fff0dc"
identifierColor RightSecondPred = "#fff0dc"
identifierColor CombineResults = "#f0eaff"
identifierColor SkipWhitespace = "#fff0dc"
identifierColor ReadToken = "#eaf2ff"
identifierColor DropToken = "#fff0dc"
identifierColor CompareToken = "#e4f4f0"
identifierColor CheckFirstArgument = "#fff0ed"
identifierColor CheckFormatFlag = "#fff0ed"
identifierColor CheckFormatValue = "#fff0ed"
identifierColor CheckTrailingInput = "#fff0ed"
identifierColor BuildResult = "#f0eaff"
identifierColor ReportError = "#ffe6e6"
identifierColor _ = "#e4f4f0"

identifierBorderColor :: Identifier -> String
identifierBorderColor ZeroBranch = "#d18a7c"
identifierBorderColor CheckZero = "#70a99e"
identifierBorderColor ReturnZero = "#7f9fce"
identifierBorderColor OneBranch = "#d18a7c"
identifierBorderColor CheckOne = "#70a99e"
identifierBorderColor ReturnOne = "#7f9fce"
identifierBorderColor LeftPred = "#c89a58"
identifierBorderColor RightFirstPred = "#c89a58"
identifierBorderColor RightSecondPred = "#c89a58"
identifierBorderColor CombineResults = "#a18bc4"
identifierBorderColor SkipWhitespace = "#c89a58"
identifierBorderColor ReadToken = "#7f9fce"
identifierBorderColor DropToken = "#c89a58"
identifierBorderColor CompareToken = "#70a99e"
identifierBorderColor CheckFirstArgument = "#d18a7c"
identifierBorderColor CheckFormatFlag = "#d18a7c"
identifierBorderColor CheckFormatValue = "#d18a7c"
identifierBorderColor CheckTrailingInput = "#d18a7c"
identifierBorderColor BuildResult = "#a18bc4"
identifierBorderColor ReportError = "#d18a7c"
identifierBorderColor _ = "#70a99e"

eventLabel :: ProvenanceEvent -> String
eventLabel (ProvenanceNat (value, expression)) =
  "Nat " ++ show value ++ "\n" ++ show expression
eventLabel (ProvenanceEqNat (left, leftExpr) (right, rightExpr)) =
  "EqNat " ++ show left ++ " == " ++ show right
    ++ "\n" ++ show leftExpr ++ " == " ++ show rightExpr
eventLabel (ProvenancePredNat (value, expression)) =
  "PredNat " ++ show value ++ "\n" ++ show expression
eventLabel (ProvenanceAddNat (left, leftExpr) (right, rightExpr)) =
  "AddNat " ++ show left ++ " + " ++ show right
    ++ "\n" ++ show leftExpr ++ " + " ++ show rightExpr
eventLabel (ProvenanceIf (condition, expression)) =
  "If " ++ show condition ++ "\n" ++ show expression
eventLabel (ProvenanceStringEmpty (value, expression)) =
  "Empty? " ++ compact value ++ "\n" ++ compact expression
eventLabel (ProvenanceHeadChar (value, expression)) =
  "Head " ++ compact value ++ "\n" ++ compact expression
eventLabel (ProvenanceTailString (value, expression)) =
  "Tail " ++ compact value ++ "\n" ++ compact expression
eventLabel (ProvenanceConsChar (c, cExpr) (s, sExpr)) =
  "Cons " ++ show c ++ " " ++ compact s ++ "\n" ++ compact cExpr ++ " : " ++ compact sExpr
eventLabel (ProvenanceChar (c, _)) = "Char " ++ show c
eventLabel ProvenanceEmptyString = "Empty string"
eventLabel (ProvenanceBool (b, _)) = "Bool " ++ show b
eventLabel (ProvenanceEqChar (a, aExpr) (b, bExpr)) =
  "EqChar " ++ show a ++ " == " ++ show b ++ "\n" ++ compact aExpr ++ " == " ++ compact bExpr
eventLabel (ProvenanceFormat (value, _)) = "Format " ++ show value
eventLabel (ProvenanceSuccess (name, _) (chosen, _)) =
  "ParseResult " ++ show name ++ " " ++ show chosen
eventLabel (ProvenanceFailure reason) = "ParseError " ++ show reason
eventLabel ProvenanceEmptyNatList = "Empty Nat list"
eventLabel (ProvenanceNatListEmpty (xs, expression)) =
  "Empty Nat list? " ++ compact xs ++ "\n" ++ compact expression
eventLabel (ProvenanceHeadNat (xs, expression)) =
  "Head Nat " ++ compact xs ++ "\n" ++ compact expression
eventLabel (ProvenanceTailNat (xs, expression)) =
  "Tail Nat " ++ compact xs ++ "\n" ++ compact expression
eventLabel (ProvenanceConsNat (n, _) (xs, _)) =
  "Cons Nat " ++ show n ++ " " ++ compact xs
eventLabel (ProvenanceLeNat (a, _) (b, _)) =
  "Le Nat " ++ show a ++ " <= " ++ show b

compact :: Show a => a -> String
compact value = case show value of
  s | length s > 42 -> take 39 s ++ "..."
    | otherwise -> s

eventColor :: ProvenanceEvent -> String
eventColor (ProvenanceNat _) = "#eaf2ff"
eventColor (ProvenanceEqNat _ _) = "#e4f4f0"
eventColor (ProvenancePredNat _) = "#fff0dc"
eventColor (ProvenanceAddNat _ _) = "#f0eaff"
eventColor (ProvenanceIf _) = "#fff0ed"
eventColor (ProvenanceStringEmpty _) = "#e4f4f0"
eventColor (ProvenanceHeadChar _) = "#fff0dc"
eventColor (ProvenanceTailString _) = "#fff0dc"
eventColor (ProvenanceConsChar _ _) = "#eaf2ff"
eventColor (ProvenanceChar _) = "#eaf2ff"
eventColor ProvenanceEmptyString = "#eaf2ff"
eventColor (ProvenanceBool _) = "#eaf2ff"
eventColor (ProvenanceEqChar _ _) = "#e4f4f0"
eventColor (ProvenanceFormat _) = "#f0eaff"
eventColor (ProvenanceSuccess _ _) = "#f0eaff"
eventColor (ProvenanceFailure _) = "#ffe6e6"
eventColor ProvenanceEmptyNatList = "#eaf2ff"
eventColor (ProvenanceNatListEmpty _) = "#e4f4f0"
eventColor (ProvenanceHeadNat _) = "#fff0dc"
eventColor (ProvenanceTailNat _) = "#fff0dc"
eventColor (ProvenanceConsNat _ _) = "#eaf2ff"
eventColor (ProvenanceLeNat _ _) = "#e4f4f0"

dotQuote :: String -> String
dotQuote value = "\"" ++ concatMap escape value ++ "\""
  where
    escape '\\' = "\\\\"
    escape '"' = "\\\""
    escape '\n' = "\\n"
    escape character = [character]

writeProvenanceGraph :: FilePath -> String -> ProvenanceTrace -> IO ()
writeProvenanceGraph directory name trace = do
  let dotFile = directory </> name ++ ".dot"
      svgFile = directory </> name ++ ".svg"
  writeFile dotFile (provenanceTraceToDot trace)
  callProcess "dot" ["-Tsvg", dotFile, "-o", svgFile]

writeGroupedProvenanceGraph :: FilePath -> String -> ProvenanceTrace -> IO ()
writeGroupedProvenanceGraph directory name trace = do
  let dotFile = directory </> name ++ "-grouped.dot"
      svgFile = directory </> name ++ "-grouped.svg"
  writeFile dotFile (groupedProvenanceTraceToDot trace)
  callProcess "dot" ["-Tsvg", dotFile, "-o", svgFile]

writeCollapsedProvenanceGraph :: FilePath -> String -> ProvenanceTrace -> IO ()
writeCollapsedProvenanceGraph directory name trace = do
  let dotFile = directory </> name ++ "-collapsed.dot"
      svgFile = directory </> name ++ "-collapsed.svg"
  writeFile dotFile (collapsedProvenanceTraceToDot trace)
  callProcess "dot" ["-Tsvg", dotFile, "-o", svgFile]

seqProvenance :: ProvenanceTrace -> ProvenanceTrace -> ProvenanceTrace
seqProvenance ProvenanceEmpty q = q
seqProvenance p ProvenanceEmpty = p
seqProvenance (ProvenanceSeq p q) r = seqProvenance p (seqProvenance q r)
seqProvenance p q = ProvenanceSeq p q

parProvenance :: ProvenanceTrace -> ProvenanceTrace -> ProvenanceTrace
parProvenance ProvenanceEmpty q = q
parProvenance p ProvenanceEmpty = p
parProvenance p q = ProvenancePar p q

provenanceAtom :: ProvenanceEvent -> ProvenanceTrace
provenanceAtom = ProvenanceAtom . IdentifiedEvent Nothing

-- The innermost annotation wins; an outer annotation fills remaining events.
labelUnmarked :: Identifier -> ProvenanceTrace -> ProvenanceTrace
labelUnmarked name = \case
  ProvenanceEmpty -> ProvenanceEmpty
  ProvenanceAtom identified -> ProvenanceAtom $ case identifier identified of
    Nothing -> identified { identifier = Just name }
    Just _ -> identified
  ProvenanceSeq left right ->
    ProvenanceSeq (labelUnmarked name left) (labelUnmarked name right)
  ProvenancePar left right ->
    ProvenancePar (labelUnmarked name left) (labelUnmarked name right)

firstExpr :: Expr (a, b) -> Expr a
firstExpr (Pair a _) = a
firstExpr expression = First expression

secondExpr :: Expr (a, b) -> Expr b
secondExpr (Pair _ b) = b
secondExpr expression = Second expression

newtype ProvenanceTraced a b = ProvenanceTraced
  { runProvenanceTraced :: Provenanced a -> (Provenanced b, ProvenanceTrace) }

runWithProvenance :: ProvenanceTraced a b -> a -> (Provenanced b, ProvenanceTrace)
runWithProvenance arrow value = runProvenanceTraced arrow (value, Input)

instance Category ProvenanceTraced where
  id = ProvenanceTraced (\a -> (a, ProvenanceEmpty))
  ProvenanceTraced g . ProvenanceTraced f = ProvenanceTraced $ \a ->
    let (b, p) = f a
        (c, q) = g b
    in (c, seqProvenance p q)

instance Cartesian ProvenanceTraced where
  exl = ProvenanceTraced $ \((a, _), expression) ->
    ((a, firstExpr expression), ProvenanceEmpty)
  exr = ProvenanceTraced $ \((_, b), expression) ->
    ((b, secondExpr expression), ProvenanceEmpty)
  terminal = ProvenanceTraced $ \_ -> (((), Unit), ProvenanceEmpty)
  ProvenanceTraced f &&& ProvenanceTraced g = ProvenanceTraced $ \a ->
    let ((bValue, bExpr), p) = f a
        ((cValue, cExpr), q) = g a
    in ( ((bValue, cValue), Pair bExpr cExpr)
       , parProvenance p q )
  nat n = ProvenanceTraced $ \_ ->
    ((n, Constant n), provenanceAtom (ProvenanceNat (n, Constant n)))
  eqNat = ProvenanceTraced $ \((a, b), expression) ->
    let a' = (a, firstExpr expression)
        b' = (b, secondExpr expression)
    in ((a == b, Equal (snd a') (snd b')),
        provenanceAtom (ProvenanceEqNat a' b'))
  predNat = ProvenanceTraced $ \input@(n, expression) ->
    ((predecessor n, Pred expression), provenanceAtom (ProvenancePredNat input))
  addNat = ProvenanceTraced $ \((a, b), expression) ->
    let a' = (a, firstExpr expression)
        b' = (b, secondExpr expression)
    in ((a + b, Add (snd a') (snd b')),
        provenanceAtom (ProvenanceAddNat a' b'))
  stringEmpty = ProvenanceTraced $ \input@(s, expression) ->
    ((null s, IsEmpty expression), provenanceAtom (ProvenanceStringEmpty input))
  headChar = ProvenanceTraced $ \input@(s, expression) ->
    ((head s, HeadChar expression), provenanceAtom (ProvenanceHeadChar input))
  tailString = ProvenanceTraced $ \input@(s, expression) ->
    ((tail s, TailString expression), provenanceAtom (ProvenanceTailString input))
  consChar = ProvenanceTraced $ \((c, s), expression) ->
    let c' = (c, firstExpr expression)
        s' = (s, secondExpr expression)
    in ((c : s, ConsChar (snd c') (snd s')),
        provenanceAtom (ProvenanceConsChar c' s'))
  char c = ProvenanceTraced $ \_ ->
    ((c, LiteralChar c), provenanceAtom (ProvenanceChar (c, LiteralChar c)))
  emptyString = ProvenanceTraced $ \_ ->
    (("", EmptyString), provenanceAtom ProvenanceEmptyString)
  bool b = ProvenanceTraced $ \_ ->
    ((b, LiteralBool b), provenanceAtom (ProvenanceBool (b, LiteralBool b)))
  eqChar = ProvenanceTraced $ \((a, b), expression) ->
    let a' = (a, firstExpr expression)
        b' = (b, secondExpr expression)
    in ((a == b, EqualChar (snd a') (snd b')),
        provenanceAtom (ProvenanceEqChar a' b'))
  formatValue value = ProvenanceTraced $ \_ ->
    ((value, LiteralFormat value), provenanceAtom (ProvenanceFormat (value, LiteralFormat value)))
  success = ProvenanceTraced $ \((name, chosen), expression) ->
    let name' = (name, firstExpr expression)
        chosen' = (chosen, secondExpr expression)
    in ((Right (ParseResult name chosen), Parsed (snd name') (snd chosen')),
        provenanceAtom (ProvenanceSuccess name' chosen'))
  failure reason = ProvenanceTraced $ \_ ->
    ((Left reason, Failed reason), provenanceAtom (ProvenanceFailure reason))
  emptyNatList = ProvenanceTraced $ \_ ->
    (([], EmptyNatList), provenanceAtom ProvenanceEmptyNatList)
  natListEmpty = ProvenanceTraced $ \input@(xs, expression) ->
    ((null xs, IsNatListEmpty expression), provenanceAtom (ProvenanceNatListEmpty input))
  headNat = ProvenanceTraced $ \input@(xs, expression) ->
    ((head xs, HeadNat expression), provenanceAtom (ProvenanceHeadNat input))
  tailNat = ProvenanceTraced $ \input@(xs, expression) ->
    ((tail xs, TailNat expression), provenanceAtom (ProvenanceTailNat input))
  consNat = ProvenanceTraced $ \((n, xs), expression) ->
    let n' = (n, firstExpr expression)
        xs' = (xs, secondExpr expression)
    in ((n : xs, ConsNat (snd n') (snd xs')),
        provenanceAtom (ProvenanceConsNat n' xs'))
  leNat = ProvenanceTraced $ \((a, b), expression) ->
    let a' = (a, firstExpr expression)
        b' = (b, secondExpr expression)
    in ((a <= b, LessEqualNat (snd a') (snd b')),
        provenanceAtom (ProvenanceLeNat a' b'))
  annotate name (ProvenanceTraced f) = ProvenanceTraced $ \input ->
    let (result, trace) = f input
    in (result, labelUnmarked name trace)
  branch (ProvenanceTraced yes) (ProvenanceTraced no) = ProvenanceTraced $
    \((condition, a), expression) ->
      let condition' = (condition, firstExpr expression)
          argument = (a, secondExpr expression)
          (b, trace) = if condition then yes argument else no argument
      in (b, seqProvenance (provenanceAtom (ProvenanceIf condition')) trace)

-- Par describes independent branches; this small interpreter does not launch
-- threads. Seq (Par p q) r records their join before r begins.
main :: IO ()
main = do
  args <- getArgs
  outputDirectory <- case args of
    [] -> pure "graphs"
    [directory] -> pure directory
    _ -> fail "Usage: trace-fibonacci [output-directory]"
  createDirectoryIfMissing True outputDirectory
  let examples = zip [0 .. 5] [0, 1, 1, 2, 3, 5]
  forM_ examples $ \(n, expected) -> do
    let normal = (fib :: Nat -> Nat) n
        (result, trace) = runTraced (fib :: Traced Nat Nat) n
        ((provenanceResult, expression), provenanceTrace) =
          runWithProvenance (fib :: ProvenanceTraced Nat Nat) n
    unless (normal == expected && result == expected && provenanceResult == expected) $
      fail ("Incorrect Fibonacci result for " ++ show n)
    putStrLn ("fib(" ++ show n ++ ") = " ++ show result)
    putStrLn ("  " ++ show trace)
    putStrLn ("  provenance result: " ++ show (provenanceResult, expression))
    putStrLn ("  provenance trace: " ++ show provenanceTrace)
    writeProvenanceGraph outputDirectory ("fib-" ++ show n) provenanceTrace
    writeCollapsedProvenanceGraph outputDirectory ("fib-" ++ show n) provenanceTrace
    writeGroupedProvenanceGraph outputDirectory ("fib-" ++ show n) provenanceTrace

  let cliExamples =
        [ ("default", "f", Right (ParseResult "f" Json))
        , ("csv", "f --format CSV", Right (ParseResult "f" CSV))
        , ("tsv", "f --format TSV", Right (ParseResult "f" TSV))
        , ("json", "f --format JSON", Right (ParseResult "f" Json))
        , ("format-first", "--format CSV f", Right (ParseResult "f" CSV))
        , ("missing-file", "", Left MissingFileName)
        , ("missing-format", "f --format", Left MissingFormat)
        , ("invalid-format", "f --format XML", Left InvalidFormat)
        , ("extra-argument", "f extra", Left UnexpectedArgument)
        , ("unknown-option", "--bogus f", Left UnexpectedArgument)
        ]
  forM_ cliExamples $ \(name, input, expected) -> do
    let normal = (parseCli :: String -> Either ParseError ParseResult) input
        (result, _) = runTraced (parseCli :: Traced String (Either ParseError ParseResult)) input
        ((provenanceResult, _), provenanceTrace) =
          runWithProvenance (parseCli :: ProvenanceTraced String (Either ParseError ParseResult)) input
    unless (normal == expected && result == expected && provenanceResult == expected) $
      fail ("Incorrect CLI parse result for " ++ show input)
    putStrLn ("parseCli " ++ show input ++ " = " ++ show result)
    writeProvenanceGraph outputDirectory ("cli-" ++ name) provenanceTrace
    writeCollapsedProvenanceGraph outputDirectory ("cli-" ++ name) provenanceTrace
    writeGroupedProvenanceGraph outputDirectory ("cli-" ++ name) provenanceTrace

  let sourceExample :: ProvenanceTraced () Bool
      sourceExample =
        (((constant 1 &&& constant 16) >>> addNat) &&& constant 20) >>> eqNat
      (sourceResult, sourceTrace) = runWithProvenance sourceExample ()
  putStrLn ("source example = " ++ show sourceResult)
  putStrLn ("  " ++ show sourceTrace)
  writeProvenanceGraph outputDirectory "source-example" sourceTrace
