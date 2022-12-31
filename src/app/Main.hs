{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

-- system
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

-- built-ins
import Data.Foldable (traverse_)
import Text.Printf (PrintfArg, PrintfType, printf)

-- algorithms
-- import HandleSplitUTF
import InlinedBSFold
import InlinedMonoidBSFold
import Lazy
import LazyUTFAgnostic
import MonoidBSFold

-- import Parallel
import Simple (simple)
import SimpleBSFold (simpleBSFold)
import SimpleFold (simpleFold)
import Strict (strictBytestream)
import Stupid (stupid)

-- types
import Types (
  Counts (Counts, charCount, lineCount, wordCount),
  getFlux,
 )

printResult ::
  (Text.Printf.PrintfArg t1, Text.Printf.PrintfType t2) =>
  (t1, Counts) ->
  t2
printResult (name, Counts{charCount, wordCount, lineCount}) =
  printf "%d %d %d %s\n" lineCount (getFlux wordCount) charCount name

main :: IO ()
-- main = putStrLn "Hello, Haskell!"
main = do
  results <-
    getArgs
      >>= \case
        -- ("handle-utf" : filenames) -> handleSplitUTF filenames
        ("inlined-bs-fold" : filenames) -> inlinedBSFold filenames
        ("inlined-monoid-bs-fold" : filenames) -> inlinedMonoidBSFold filenames
        ("lazy-utf" : filenames) -> lazyUTF8 filenames
        ("lazy" : filenames) -> lazyBytestream filenames
        ("monoid-bs-fold" : filenames) -> monoidBSFold filenames
        ("simple-bs-fold" : filenames) -> simpleBSFold filenames
        ("simple-fold" : filenames) -> simpleFold filenames
        ("simple" : filenames) -> simple filenames
        ("strict" : filenames) -> strictBytestream filenames
        ("stupid" : filenames) -> stupid filenames
        _ -> hPutStrLn stderr "usage: <simple|lazy> [files...]" >> exitFailure
  traverse_ printResult results
