# Describing the Technique
```haskell
  -- Proposed interfaces, not existing definitions:
  features :: Trace -> Set Feature
  generate :: SearchState -> Gen Nat
  mutate   :: SearchState -> Nat -> Trace -> Gen Nat
```

the idea is to perform coverage-guided property-based testing of code using trace-aware mutators. The traces would come from interpreting programs in other categories. For example, if we were to test an implementation of the Fibonacci function, then one trace we could collect is the following: 

```
fib(0) = 0 [DoNat 0 ; DoEqNat 0 0 ; DoIf True ; DoNat 0]
fib(1) = 1 [DoNat 0 ; DoEqNat 1 0 ; DoIf False ; DoNat 1 ; DoEqNat 1 1 ; DoIf True ; DoNat 1]
fib(2) = 1 [DoNat 0 ; DoEqNat 2 0 ; DoIf False ; DoNat 1 ; DoEqNat 2 1 ; DoIf False ; ([DoPredNat 2 ; DoNat 0 ; DoEqNat 1 0 ; DoIf False ; DoNat 1 ; DoEqNat 1 1 ; DoIf True ; DoNat 1] | [DoPredNat 2 ; DoPredNat 1 ; DoNat 0 ; DoEqNat 0 0 ; DoIf True ; DoNat 0]) ; DoAddNat 1 0]
fib(3) = 2 [DoNat 0 ; DoEqNat 3 0 ; DoIf False ; DoNat 1 ; DoEqNat 3 1 ; DoIf False ; ([DoPredNat 3 ; DoNat 0 ; DoEqNat 2 0 ; DoIf False ; DoNat 1 ; DoEqNat 2 1 ; DoIf False ; ([DoPredNat 2 ; DoNat 0 ; DoEqNat 1 0 ; DoIf False ; DoNat 1 ; DoEqNat 1 1 ; DoIf True ; DoNat 1] | [DoPredNat 2 ; DoPredNat 1 ; DoNat 0 ; DoEqNat 0 0 ; DoIf True ; DoNat 0]) ; DoAddNat 1 0] | [DoPredNat 3 ; DoPredNat 2 ; DoNat 0 ; DoEqNat 1 0 ; DoIf False ; DoNat 1 ; DoEqNat 1 1 ; DoIf True ; DoNat 1]) ; DoAddNat 1 1]
```

In this example, the trace of fib(0) and fib(1) is contained inside the trace of fib(2), so heuristically,
one might say that fib(2) is "more interesting" (in a loose sense) from a testing perspective than fib(0) or fib(1), although
there are multiple ways to measure the features of a trace. (Although I get that Fibonacci is a toy-example
that could be tested naively, but I just chose a simple example to illustrate the technique)

The idea is to use the trace to determine which samples are "more likely to trigger a bug" vs "less likely to trigger a bug", and then use trace-aware mutators to mutate samples in the corpus (interesting samples are mutated, uninteresting samples are pruned from the corpus).

A different research direction is to make the generators aware of traces of previous generations.


# Naive blackbox vs tracing vs semantic whitebox
The goal of this project is to explore a middle-ground between naive blackbox sampling and whitebox semantic
sampling.
On one end, we have naive blackbox sample that just samples without knowing anything about the specific function.
On the other end, we have whitebox semantic sampling that reads the function sourcecode to custom sample.
Our approach does not read the function sourcecode. The only information it receives about the function under
testing is from the traces. 

# The benefit of this approach
The benefit of this approach over traditional quickecheck
is that in traditional quickcheck, the user must
write custom domain-specific generators in certain situations
(in order to achieve reasonable coverage).

We define an alternate approach: the user defines 
domain-specific mutators (easier than domain-specific generators),
and the trace-aware framework will automatically handle the rest. The user can also seed the corpus 
with examples (like unit tests).
In short, naive blackbox generator + trace-aware mutator should approximate a domain-specific generator.

## Example: expression interpretor example. 
Example: imagine an interpretor of simple expressions 
```haskell
interpret :: Expr -> Either TypeError InterpretationResult
```
1. Under naive blackbox quickcheck, most samples result in `TypeError`, leaving most paths untested.
2. Under bespoke hand-written quickcheck, the user must write a generator that generates well-typed expressions (can be cumbersome)
3. Under our approach, the user can seed the corpus with some well-typed expressions and define simple mutators (each mutator will be simpler to write than a wholesale generator) and the trace-guided framework will automatically choose which samples to add to or prune from the corpus.

# Research Question
Can a trace-aware or a trace-guided version of quickcheck lead to more sample-efficient 
counter-example synthesis for QuickCheck properties, as opposed to naive QuickCheck? 

Does the structure or content of traces provided make a difference in how sample-efficient quickcheck is? 
For example, a trace could be represented as a sequential list of operations, or a multiset which 
counts how many times each operation has been called, or a set of which operations have been called.
Which one works best? 


# Example Domains
Here are some example domains where we hypothesize that trace-guided counterexample synthesis could be used to 
improve sampling, leading to more interesting paths being exercised: 

- A library for parsing command-line flags and arguments. For example, imagine the CLI tool has a bug that is only triggered when it is passed `--format JSON`. A naive sampler would be unlikely to generate a counter-example. The typical solution for QuickCheck in this case would be to write a custom domain-aware generator.
- An implementation of LZ77 Lempel Ziv compression and decompression.
- A client-server state-machine protocol. (Use quickcheck to generate examples of request-response sequences).

In each of these examples, we aim to show that naive random sampling (a la QuickCheck) would
mostly generate uninteresting samples, and that some code-paths would be exercised much more than other
code-paths, and furthermore, we aim to show that trace-guided counterexample synthesis could make the sampling
processes more sample-efficient. In other words, that a trace-guided generator could discover a counter-example
in fewer samples than a naive generator.

# Previous work

- FuzzChick combines property testing with coverage-guided mutation of structured inputs. FuzzChick
  found benefits from coverage-guided mutation
- JQF combines generators with execution feedback.
- Guiding generators using execution feedback is  established in Zest. Zest found benefits from using validity feedback with structured generators
- REDQUEEN exploits relationships between inputs and runtime values, while
- IJON supports guidance through internal program state.


# CLI Case study.

Imagine a CLI tool that 
```haskell
Format = CSV | TSV | Json

ParseResult = 
  { fileName :: String
  , format :: Format
  }

ParseError = MissingFileName | MissingFormat | InvalidFormat | UnexpectedArgument

parseCli :: String -> (Either ParseError ParseResult)
```
The argument `--format <FORMAT>` is optional, and defaults to `JSON`. The positional argument `<FILE_NAME>` is required.
  Build one end-to-end vertical slice on the CLI example: ordinary QuickCheck property, default mutator, trace features, bounded corpus, replay, and shrinking. Then compare it across many
  seeds against 
  (1) plain naive blackbox QuickCheck, 
  (2) QuickCheck with a bespoke domain-specific hand-written generator, and 
  (3) the same mutation engine using only branch-outcome features.
  
  Repeat on a less hand-crafted
  domain.

# Example of Trace

```
fib(0) = 0
  [DoNat 0 ; DoEqNat 0 0 ; DoIf True ; DoNat 0]
  provenance result: (0,0)
  provenance trace: [ProvenanceNat (0,0) ; ProvenanceEqNat (0,input) (0,0) ; ProvenanceIf (True,input==0) ; ProvenanceNat (0,0)]
fib(1) = 1
  [DoNat 0 ; DoEqNat 1 0 ; DoIf False ; DoNat 1 ; DoEqNat 1 1 ; DoIf True ; DoNat 1]
  provenance result: (1,1)
  provenance trace: [ProvenanceNat (0,0) ; ProvenanceEqNat (1,input) (0,0) ; ProvenanceIf (False,input==0) ; ProvenanceNat (1,1) ; ProvenanceEqNat (1,input) (1,1) ; ProvenanceIf (True,input==1) ; ProvenanceNat (1,1)]
fib(2) = 1
  [DoNat 0 ; DoEqNat 2 0 ; DoIf False ; DoNat 1 ; DoEqNat 2 1 ; DoIf False ; ([DoPredNat 2 ; DoNat 0 ; DoEqNat 1 0 ; DoIf False ; DoNat 1 ; DoEqNat 1 1 ; DoIf True ; DoNat 1] | [DoPredNat 2 ; DoPredNat 1 ; DoNat 0 ; DoEqNat 0 0 ; DoIf True ; DoNat 0]) ; DoAddNat 1 0]
  provenance result: (1,1+0)
  provenance trace: [ProvenanceNat (0,0) ; ProvenanceEqNat (2,input) (0,0) ; ProvenanceIf (False,input==0) ; ProvenanceNat (1,1) ; ProvenanceEqNat (2,input) (1,1) ; ProvenanceIf (False,input==1) ; ([ProvenancePredNat (2,input) ; ProvenanceNat (0,0) ; ProvenanceEqNat (1,pred(input)) (0,0) ; ProvenanceIf (False,pred(input)==0) ; ProvenanceNat (1,1) ; ProvenanceEqNat (1,pred(input)) (1,1) ; ProvenanceIf (True,pred(input)==1) ; ProvenanceNat (1,1)] | [ProvenancePredNat (2,input) ; ProvenancePredNat (1,pred(input)) ; ProvenanceNat (0,0) ; ProvenanceEqNat (0,pred(pred(input))) (0,0) ; ProvenanceIf (True,pred(pred(input))==0) ; ProvenanceNat (0,0)]) ; ProvenanceAddNat (1,1) (0,0)]
fib(3) = 2
  [DoNat 0 ; DoEqNat 3 0 ; DoIf False ; DoNat 1 ; DoEqNat 3 1 ; DoIf False ; ([DoPredNat 3 ; DoNat 0 ; DoEqNat 2 0 ; DoIf False ; DoNat 1 ; DoEqNat 2 1 ; DoIf False ; ([DoPredNat 2 ; DoNat 0 ; DoEqNat 1 0 ; DoIf False ; DoNat 1 ; DoEqNat 1 1 ; DoIf True ; DoNat 1] | [DoPredNat 2 ; DoPredNat 1 ; DoNat 0 ; DoEqNat 0 0 ; DoIf True ; DoNat 0]) ; DoAddNat 1 0] | [DoPredNat 3 ; DoPredNat 2 ; DoNat 0 ; DoEqNat 1 0 ; DoIf False ; DoNat 1 ; DoEqNat 1 1 ; DoIf True ; DoNat 1]) ; DoAddNat 1 1]
  provenance result: (2,1+0+1)
  provenance trace: [ProvenanceNat (0,0) ; ProvenanceEqNat (3,input) (0,0) ; ProvenanceIf (False,input==0) ; ProvenanceNat (1,1) ; ProvenanceEqNat (3,input) (1,1) ; ProvenanceIf (False,input==1) ; ([ProvenancePredNat (3,input) ; ProvenanceNat (0,0) ; ProvenanceEqNat (2,pred(input)) (0,0) ; ProvenanceIf (False,pred(input)==0) ; ProvenanceNat (1,1) ; ProvenanceEqNat (2,pred(input)) (1,1) ; ProvenanceIf (False,pred(input)==1) ; ([ProvenancePredNat (2,pred(input)) ; ProvenanceNat (0,0) ; ProvenanceEqNat (1,pred(pred(input))) (0,0) ; ProvenanceIf (False,pred(pred(input))==0) ; ProvenanceNat (1,1) ; ProvenanceEqNat (1,pred(pred(input))) (1,1) ; ProvenanceIf (True,pred(pred(input))==1) ; ProvenanceNat (1,1)] | [ProvenancePredNat (2,pred(input)) ; ProvenancePredNat (1,pred(pred(input))) ; ProvenanceNat (0,0) ; ProvenanceEqNat (0,pred(pred(pred(input)))) (0,0) ; ProvenanceIf (True,pred(pred(pred(input)))==0) ; ProvenanceNat (0,0)]) ; ProvenanceAddNat (1,1) (0,0)] | [ProvenancePredNat (3,input) ; ProvenancePredNat (2,pred(input)) ; ProvenanceNat (0,0) ; ProvenanceEqNat (1,pred(pred(input))) (0,0) ; ProvenanceIf (False,pred(pred(input))==0) ; ProvenanceNat (1,1) ; ProvenanceEqNat (1,pred(pred(input))) (1,1) ; ProvenanceIf (True,pred(pred(input))==1) ; ProvenanceNat (1,1)]) ; ProvenanceAddNat (1,1+0) (1,1)]
```

# Stitching Traces
If we see that sample `x` produces trace `[A ; B ; C]` and sample 
`y` produces trace `[X ; B ; Y]`, we should not assume that the traces
`[A ; B ; Y]` or `[X ; B ; C]` are possible.
You can't just stitch together two traces. We can only
conclude that a trace exists if a concrete input to the
function produces that trace.

# Challenging Examples/scenarios for trace-aware quickcheck
These are some challenging use-cases that I
haven't figured out yet.

- Authenticated message parser: `(ByteString, MAC)`. What the traces would show Nearly every random input reaches `InvalidMAC` and stops.
- Typed language interpreter: `Expr`. Most inputs would have `TypeError`.

# Repository Rules
You are never allowed to read `ai_prompts.md`

# Trace Families
We define a trace family by quotienting the set
of traces modulo their collapsed graphs.
Two traces are in the "same family" if they have
equal collapsed graphs.
We can summarize a trace with (pseudo-code, not real code)
```haskell 
summary Empty = Summary ∅ ∅ ∅ ∅

summary (Atom atom) =
  let a = identifier atom
  in Summary { nodes = {a}, first = {a}, last = {a}, follows = ∅ }

summary (Par p q) =
  let x = summary p
      y = summary q
  in Summary
       (nodes x ∪ nodes y)
       (first x ∪ first y)
       (last x ∪ last y)
       (follows x ∪ follows y)

summary (Seq p q) =
  let x = summary p
      y = summary q
  in Summary
       (nodes x ∪ nodes y)
       (first x)
       (last y)
       (follows x ∪ follows y ∪ { (a, b) | a ∈ last x, b ∈ first y })
```
For trace families, we only care about edge-presence or edge-absence, but not the count of edges.
For example, for the Fibonacci example, there are exactly 4 unique trace-families:
1. `{fib(0)}`
2. `{fib(1)}`
3. `{fib(2)}`
4. `{fib(3), fib(4), fib(5), ..., }`
