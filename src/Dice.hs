{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Dice(
  Die
) where

import Data.List
import Data.Maybe
import Data.Ratio
import Control.Applicative

class (Eq (Carried d), Ord (Carried d)) => Die d where
  type Carried d = c | c -> d
  possible :: d -> [Carried d]
  actions :: d -> Maybe (Carried d -> [d])
  mkSimpleDie :: [Carried d] -> d

data Die' c = Die' { act' :: Maybe (c -> [Die' c]), poss' :: [c] }

instance Die (Die' Integer) where
  type Carried (Die' Integer) = Integer
  possible = poss'
  actions = act'
  mkSimpleDie = Die' Nothing

instance Die (Die' Int) where
  type Carried (Die' Int) = Int
  possible = poss'
  actions = act'
  mkSimpleDie = Die' Nothing

instance Die (Die' Char) where
  type Carried (Die' Char) = Char
  possible = poss'
  actions = act'
  mkSimpleDie = Die' Nothing

parseXdY :: Die d => String -> [d]
parseXdY = undefined --TODO

options :: (Die d, Die e) => d -> e -> [(Carried d, Carried e)] --This is just the cartesian product of two lists
options d e = (,) <$> possible d <*> possible e

options3 :: (Die d, Die e, Die f) => d -> e -> f -> [(Carried d, Carried e,Carried f)] --This is just the cartesian product of three lists
options3 d e f = (,,) <$> possible d <*> possible e <*> possible f

applyOp :: (Die d, Die e, Die f) => (Carried d -> Carried e -> Carried f) -> [(Carried d, Carried e)] -> [Carried f]
applyOp f ((a,b) : xs)
  | null xs = []
  | otherwise = a `f` b : applyOp f xs

getStats :: Die d => [Carried d] -> [(Carried d,Integer)]
getStats x = map (\l -> (head l, fromIntegral $ length l)) . group $ sort x

chanceOf :: Die d => Carried d -> [(Carried d,Integer)] -> Maybe Rational
chanceOf r x
  | isNothing chance = Nothing
  | otherwise = Just $ snd (fromJust chance) % tot
    where chance = find (\l -> fst l == r) x
          tot = sum $ map snd x

--Standard Dice

dZ :: Integer -> Die' Integer -- | Z is used as an obtuse reference to the set of Integers
dZ z = mkSimpleDie [1..z]

d4 :: Die' Integer
d4 = mkSimpleDie [1..4]

d6 :: Die' Integer
d6 = mkSimpleDie [1..6]

d8 :: Die' Integer
d8 = mkSimpleDie [1..8]

d10 :: Die' Integer
d10 = mkSimpleDie [1..10]

d10' :: Die' Integer
d10' = mkSimpleDie [0..9]

d12 :: Die' Integer
d12 = mkSimpleDie [1..12]

d20 :: Die' Integer
d20 = mkSimpleDie [1..20]

d100 :: Die' Integer
d100 = mkSimpleDie [1..100]

dFate :: Die' Char
dFate = mkSimpleDie ['-','-',' ', ' ','+', '+']

--TODO: Specialise explode if explodesOn is a singleton list
--TODO: Make this more polymorphic?
--Specifically, You can use rem/div to find depth and final chance trivially
--N.B. The explode function requires sorted lists as inputs
explode :: [Integer] -> [Integer] -> Integer -> Rational
explode die explodesOn target
  | target < head die = 0
  | last die > target = (1 % l) * explode (map (l + ) die) explodesOn target
  | otherwise = fromJust $ chanceOf target (getStats die)
  where l = fromIntegral $ length die

-- arsStress :: (Die d, Num a, Enum a, Carried d ~ a) => Integer -> Carried d -> d
-- arsStress nb v
--   | v == 1 = mkSimpleDie $ map (2*) [1..10]
--   | otherwise = mkSimpleDie [1..10]