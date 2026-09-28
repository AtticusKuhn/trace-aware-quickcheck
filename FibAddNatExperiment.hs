-- Build and run inside `nix develop`:
--   ghc -O1 -Wall -Werror -main-is FibAddNatExperiment \
--     -o fib-addnat-experiment FibAddNatExperiment.hs Main.hs
--   ./fib-addnat-experiment [max-n [output-directory]]
-- The default is n = 0..15, with output in graphs/addnat.
module FibAddNatExperiment (main) where

import Control.Monad (when)
import Main (Event (..), Nat, Traced, countTrace, fib, runTraced)
import System.Directory (createDirectoryIfMissing, withCurrentDirectory)
import System.Environment (getArgs)
import System.FilePath ((</>))
import System.Process (callProcess)
import Text.Read (readMaybe)

isAddNat :: Event -> Bool
isAddNat (DoAddNat _ _) = True
isAddNat _ = False

measure :: Int -> (Int, Int)
measure n =
  let (_, trace) = runTraced (fib :: Traced Nat Nat) (fromIntegral n)
  in (n, countTrace isAddNat trace)

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

-- gnuplot handles both nonlinear least-squares fitting and SVG rendering.
-- log(x+1) includes n=0; the exponential model fits its growth rate.
gnuplotScript :: Int -> String
gnuplotScript maxN = unlines
  [ "set fit logfile 'fit.log'"
  , "set fit quiet"
  , "linear(x) = linear_a + linear_b*x"
  , "linear_a = 1.0; linear_b = 1.0"
  , "fit linear(x) 'counts.tsv' using 1:2 via linear_a,linear_b"
  , "linear_sse = FIT_WSSR"
  , "quadratic(x) = quadratic_a + quadratic_b*x + quadratic_c*x*x"
  , "quadratic_a = 1.0; quadratic_b = 1.0; quadratic_c = 1.0"
  , "fit quadratic(x) 'counts.tsv' using 1:2 via quadratic_a,quadratic_b,quadratic_c"
  , "quadratic_sse = FIT_WSSR"
  , "logarithmic(x) = logarithmic_a + logarithmic_b*log(x+1)"
  , "logarithmic_a = 1.0; logarithmic_b = 1.0"
  , "fit logarithmic(x) 'counts.tsv' using 1:2 via logarithmic_a,logarithmic_b"
  , "logarithmic_sse = FIT_WSSR"
  , "exponential(x) = exponential_a*exp(exponential_b*x) + exponential_c"
  , "exponential_a = 0.7; exponential_b = 0.48; exponential_c = -1.0"
  , "fit exponential(x) 'counts.tsv' using 1:2 via exponential_a,exponential_b,exponential_c"
  , "exponential_sse = FIT_WSSR"
  , "set print 'fits.tsv'"
  , "print 'model\ta\tb\tc\tsse'"
  , "print sprintf('linear\t%.12g\t%.12g\t0\t%.12g', linear_a,linear_b,linear_sse)"
  , "print sprintf('quadratic\t%.12g\t%.12g\t%.12g\t%.12g', quadratic_a,quadratic_b,quadratic_c,quadratic_sse)"
  , "print sprintf('logarithmic\t%.12g\t%.12g\t0\t%.12g', logarithmic_a,logarithmic_b,logarithmic_sse)"
  , "print sprintf('exponential\t%.12g\t%.12g\t%.12g\t%.12g', exponential_a,exponential_b,exponential_c,exponential_sse)"
  , "set print"
  , "set terminal svg size 1100,700 enhanced font 'sans,12'"
  , "set output 'addnat.svg'"
  , "set title 'DoAddNat calls in fib(n)'"
  , "set xlabel 'n'"
  , "set ylabel 'DoAddNat count'"
  , "set xrange [0:" ++ show maxN ++ "]"
  , "set key left top"
  , "set grid"
  , "plot 'counts.tsv' using 1:2 with points pt 7 ps 1.2 title 'Observed', linear(x) with lines lw 2 title 'Linear', quadratic(x) with lines lw 2 title 'Quadratic', logarithmic(x) with lines lw 2 title 'Logarithmic', exponential(x) with lines lw 2 title 'Exponential'"
  , "unset output"
  , "set terminal pngcairo size 1100,700 enhanced font 'sans,12'"
  , "set output 'addnat.png'"
  , "replot"
  , "unset output"
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
  let measurements = map measure [0 .. maxN]
  writeFile (outputDirectory </> "counts.tsv") $
    unlines ("n\taddnat_count" : [show n ++ "\t" ++ show count | (n, count) <- measurements])
  writeFile (outputDirectory </> "fit.gnuplot") (gnuplotScript maxN)
  withCurrentDirectory outputDirectory $ callProcess "gnuplot" ["fit.gnuplot"]
  putStrLn "Observed DoAddNat counts:"
  mapM_ print measurements
  putStrLn "\nFitted coefficients and sum of squared errors:"
  readFile (outputDirectory </> "fits.tsv") >>= putStrLn
  putStrLn ("Graphs: " ++ outputDirectory </> "addnat.svg")
  putStrLn ("        " ++ outputDirectory </> "addnat.png")
