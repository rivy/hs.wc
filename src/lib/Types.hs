{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE UnboxedSums #-}

module Types where

import Data.Bits
import Data.ByteString.Internal (c2w, isSpaceChar8)

-- import Data.Char
-- import Data.Monoid

data Counts = Counts
  { charCount :: {-# UNPACK #-} !Int,
    wordCount :: !Flux,
    lineCount :: {-# UNPACK #-} !Int
  }
  deriving (Show)

instance Semigroup Counts where
  (Counts a b c) <> (Counts a' b' c') = Counts (a + a') (b <> b') (c + c')

instance Monoid Counts where
  mempty = Counts 0 mempty 0

data Pair a = Pair !a !a
  deriving (Show, Eq)

data CharType = IsSpace | NotSpace
  deriving (Show)

data Flux
  = Flux
      !CharType
      {-# UNPACK #-} !Int
      !CharType
  | Unknown
  deriving (Show)

instance Semigroup Flux where
  Unknown <> x = x
  x <> Unknown = x
  Flux l n NotSpace <> Flux NotSpace n' r = Flux l (n + n' - 1) r
  Flux l n _ <> Flux _ n' r = Flux l (n + n') r

instance Monoid Flux where
  mempty = Unknown

flux :: Char -> Flux
flux c
  | isSpaceChar8 c = Flux IsSpace 0 IsSpace
  | otherwise = Flux NotSpace 1 NotSpace
{-# INLINE flux #-}

countChar :: Char -> Counts
countChar c =
  Counts
    { charCount = 1,
      wordCount = flux c,
      lineCount = if (c == '\n') then 1 else 0
    }
{-# INLINE countChar #-}

getFlux :: Flux -> Int
getFlux (Flux _ n _) = n

fromTuple :: (Int, Int, Int) -> Counts
fromTuple (cs, ws, ls) =
  Counts
    { charCount = cs,
      wordCount = Flux IsSpace ws IsSpace,
      lineCount = ls
    }

toTuple :: Counts -> (Int, Int, Int)
toTuple Counts {charCount, wordCount, lineCount} = (lineCount, getFlux wordCount, charCount)

countByteUTF8 :: Char -> Counts
countByteUTF8 c
  | isContinutation = mempty
  | otherwise =
      Counts
        { -- Only count bytes at the START of a codepoint, not continuations
          charCount = if isContinutation then 0 else 1,
          -- charCount = 1
          wordCount = if isContinutation then mempty else flux c,
          lineCount = if (c == '\n') then 1 else 0
        }
  where
    isContinutation = bitAt 7 && not (bitAt 6)
    bitAt = testBit (c2w c)
{-# INLINE countByteUTF8 #-}

countByte :: Char -> Counts
countByte c =
  Counts
    { charCount = 1,
      wordCount = flux c,
      lineCount = if (c == '\n') then 1 else 0
    }
{-# INLINE countByte #-}
