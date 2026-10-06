-- Build and run inside `nix develop`:
--   ghc -O1 -Wall -Werror -main-is FibAddNatExperiment \
--     -o fib-addnat-experiment FibAddNatExperiment.hs Main.hs
--   ./fib-addnat-experiment [max-n [output-directory]]
-- The default is n = 0..15, with output in graphs/addnat.
module FibAddNatExperiment (main) where

import Control.Exception (evaluate)
import Control.Monad (replicateM_, when)
import Data.IORef (IORef, newIORef, readIORef)
import Data.List (sort)
import GHC.Clock (getMonotonicTimeNSec)
import Main (Event (..), Nat, Traced, countTrace, fib, runTraced)
import System.Directory (createDirectoryIfMissing, withCurrentDirectory)
import System.Environment (getArgs)
import System.FilePath ((</>))
import System.Process (callProcess)
import Text.Read (readMaybe)

isAddNat :: Event -> Bool
isAddNat (DoAddNat _ _) = True
isAddNat _ = False

countAddNat :: Int -> Int
countAddNat n =
  let (_, trace) = runTraced (fib :: Traced Nat Nat) (fromIntegral n)
  in countTrace isAddNat trace

normalFib :: Nat -> Nat
normalFib = fib
{-# NOINLINE normalFib #-}

-- Read the input on every iteration so the optimizer cannot share one
-- evaluation across the batch. Subtract the same loop's identity-function
-- cost to estimate the time spent in the ordinary (->) Fibonacci call.
timedBatch :: IORef Nat -> Int -> (Nat -> Nat) -> IO Double
timedBatch input repetitions function = do
  start <- getMonotonicTimeNSec
  replicateM_ repetitions $ do
    n <- readIORef input
    _ <- evaluate (function n)
    pure ()
  end <- getMonotonicTimeNSec
  pure (fromIntegral (end - start) / fromIntegral repetitions / 1000)
{-# NOINLINE timedBatch #-}

-- Report the median of five baseline-corrected batches in microseconds/call.
measureRuntime :: Int -> IO (Int, Double, Int)
measureRuntime n = do
  input <- newIORef (fromIntegral n)
  _ <- evaluate (normalFib (fromIntegral n))
  repetitions <- chooseRepetitions input 1
  samples <- mapM (\_ -> do
    baseline <- timedBatch input repetitions id
    runtime <- timedBatch input repetitions normalFib
    pure (max 0 (runtime - baseline))) [1 :: Int .. 5]
  pure (n, sort samples !! 2, repetitions)
  where
    chooseRepetitions input repetitions = do
      durationPerCall <- timedBatch input repetitions normalFib
      if durationPerCall * fromIntegral repetitions >= 20000 || repetitions >= 1048576
        then pure repetitions
        else chooseRepetitions input (repetitions * 2)

parseArgs :: [String] -> Maybe (Int, FilePath)
parseArgs args = case args of
  [] -> Just (15, "graphs/addnat")
  [upper] -> do
    n <- readMaybe upper
    if n >= 5 && n <= 20 then Just (n, "graphs/addnat") else Nothing
  [upper, directory] -> do
    n <- readMaybe upper
    if n >= 5 && n <= 20 then Just (n, directory) else Nothing
  _ -> Nothing

-- The same models are fitted to both measurements. Normalized RMSE divides
-- RMSE by the observed range, making errors comparable across units.
fitAndPlot :: Int -> FilePath -> FilePath -> FilePath -> String -> String -> String
fitAndPlot maxN dataFile fitsFile graphName title yLabel = unlines
  [ "stats '" ++ dataFile ++ "' using 2 nooutput"
  , "linear(x) = linear_a + linear_b*x"
  , "linear_a = STATS_min + 0.01*(STATS_max-STATS_min); linear_b = (STATS_max-STATS_min)/" ++ show maxN
  , "fit linear(x) '" ++ dataFile ++ "' using 1:2 via linear_a,linear_b"
  , "linear_sse = FIT_WSSR"
  , "quadratic(x) = quadratic_a + quadratic_b*x + quadratic_c*x*x"
  , "quadratic_a = STATS_min + 0.01*(STATS_max-STATS_min); quadratic_b = 0.1; quadratic_c = STATS_max/" ++ show (maxN * maxN)
  , "fit quadratic(x) '" ++ dataFile ++ "' using 1:2 via quadratic_a,quadratic_b,quadratic_c"
  , "quadratic_sse = FIT_WSSR"
  , "logarithmic(x) = logarithmic_a + logarithmic_b*log(x+1)"
  , "logarithmic_a = STATS_min + 0.01*(STATS_max-STATS_min); logarithmic_b = STATS_max/log(" ++ show (maxN + 1) ++ ")"
  , "fit logarithmic(x) '" ++ dataFile ++ "' using 1:2 via logarithmic_a,logarithmic_b"
  , "logarithmic_sse = FIT_WSSR"
  , "exponential(x) = exponential_a*exp(exponential_b*x) + exponential_c"
  , "exponential_a = STATS_max/exp(0.48*" ++ show maxN ++ "); exponential_b = 0.48; exponential_c = STATS_min + 0.01*(STATS_max-STATS_min)"
  , "fit exponential(x) '" ++ dataFile ++ "' using 1:2 via exponential_a,exponential_b,exponential_c"
  , "exponential_sse = FIT_WSSR"
  , "nrmse(sse) = sqrt(sse/" ++ show (maxN + 1) ++ ")/(STATS_max-STATS_min)"
  , "set print '" ++ fitsFile ++ "'"
  , "print 'model\ta\tb\tc\tsse\tnrmse'"
  , "print sprintf('linear\t%.12g\t%.12g\t0\t%.12g\t%.12g', linear_a,linear_b,linear_sse,nrmse(linear_sse))"
  , "print sprintf('quadratic\t%.12g\t%.12g\t%.12g\t%.12g\t%.12g', quadratic_a,quadratic_b,quadratic_c,quadratic_sse,nrmse(quadratic_sse))"
  , "print sprintf('logarithmic\t%.12g\t%.12g\t0\t%.12g\t%.12g', logarithmic_a,logarithmic_b,logarithmic_sse,nrmse(logarithmic_sse))"
  , "print sprintf('exponential\t%.12g\t%.12g\t%.12g\t%.12g\t%.12g', exponential_a,exponential_b,exponential_c,exponential_sse,nrmse(exponential_sse))"
  , "set print"
  , "set terminal svg size 1100,700 enhanced font 'sans,12'"
  , "set output '" ++ graphName ++ ".svg'"
  , "set title '" ++ title ++ "'"
  , "set xlabel 'n'"
  , "set ylabel '" ++ yLabel ++ "'"
  , "set xrange [0:" ++ show maxN ++ "]"
  , "set key left top"
  , "set grid"
  , "plot '" ++ dataFile ++ "' using 1:2 with points pt 7 ps 1.2 title 'Observed', linear(x) with lines lw 2 title 'Linear', quadratic(x) with lines lw 2 title 'Quadratic', logarithmic(x) with lines lw 2 title 'Logarithmic', exponential(x) with lines lw 2 title 'Exponential'"
  , "unset output"
  , "set terminal pngcairo size 1100,700 enhanced font 'sans,12'"
  , "set output '" ++ graphName ++ ".png'"
  , "replot"
  , "unset output"
  ]

gnuplotScript :: Int -> String
gnuplotScript maxN = unlines
  [ "set fit logfile 'fit.log'"
  , "set fit quiet"
  , fitAndPlot maxN "counts.tsv" "fits.tsv" "addnat"
      "DoAddNat calls in fib(n)" "DoAddNat count"
  , fitAndPlot maxN "runtime.tsv" "runtime-fits.tsv" "runtime"
      "Wall-clock runtime of ordinary fib(n)" "Runtime per call (microseconds)"
  ]

main :: IO ()
main = do
  args <- getArgs
  (maxN, outputDirectory) <- case parseArgs args of
    Just settings -> pure settings
    Nothing -> fail "Usage: fib-addnat-experiment [max-n (5..20) [output-directory]]"
  when (null outputDirectory) $
    fail "Output directory must not be empty"
  createDirectoryIfMissing True outputDirectory
  let counts = [(n, countAddNat n) | n <- [0 .. maxN]]
  runtimes <- mapM measureRuntime [0 .. maxN]
  writeFile (outputDirectory </> "counts.tsv") $
    unlines ("n\taddnat_count" : [show n ++ "\t" ++ show count | (n, count) <- counts])
  writeFile (outputDirectory </> "runtime.tsv") $
    unlines ("n\truntime_us\trepetitions" :
      [show n ++ "\t" ++ show runtime ++ "\t" ++ show repetitions
        | (n, runtime, repetitions) <- runtimes])
  writeFile (outputDirectory </> "fit.gnuplot") (gnuplotScript maxN)
  withCurrentDirectory outputDirectory $ callProcess "gnuplot" ["fit.gnuplot"]
  putStrLn "Observed DoAddNat counts:"
  mapM_ print counts
  putStrLn "\nObserved ordinary-arrow runtimes (n, microseconds/call, repetitions):"
  mapM_ print runtimes
  putStrLn "\nDoAddNat fits (SSE and normalized RMSE):"
  readFile (outputDirectory </> "fits.tsv") >>= putStrLn
  putStrLn "Runtime fits (SSE and normalized RMSE):"
  readFile (outputDirectory </> "runtime-fits.tsv") >>= putStrLn
  putStrLn ("Graphs: " ++ outputDirectory </> "addnat.svg")
  putStrLn ("        " ++ outputDirectory </> "addnat.png")
  putStrLn ("        " ++ outputDirectory </> "runtime.svg")
  putStrLn ("        " ++ outputDirectory </> "runtime.png")
