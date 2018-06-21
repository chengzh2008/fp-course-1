{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RebindableSyntax #-}

module Course.Core(
  Eq(..)
, Ord(..)
, Show(..)
, Integral(..)
, RealFrac(..)
, Num(..)
, Fractional(..)
, Bool(..)
, Either(..)
, Int
, Integer
, IO
, Rational
, seq
, error
, undefined
, const
, flip
, curry
, uncurry
, id
, otherwise
, (.)
, ($)
, (&&)
, (||)
, not
, even
, odd
, fst
, snd
, getChar
, on
, first
, second
, (&&&)
, (***)
, IsString(..)
, module Data.Char
, ifThenElse
, bool
) where


import Prelude
       (Bool(..), Char, Either(..), Eq(..), Fractional(..), IO, Int,
        Integer, Integral(..), Num(..), Ord(..), Rational, RealFrac(..),
        Show(..), ($), (&&), (.), (||), const, curry, error, even, flip,
        fst, id, not, odd, otherwise, seq, snd, uncurry, undefined)
import Data.String (IsString(..))
import System.IO (getChar)
import Data.Function (on)
import Control.Arrow ((&&&), (***), first, second)
import Data.Char

ifThenElse :: Bool -> a -> a -> a
ifThenElse True t _ = t
ifThenElse False _ f = f

bool :: a -> a -> Bool -> a
bool f _ False = f
bool _ t True = t
