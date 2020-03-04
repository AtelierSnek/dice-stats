{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Dice(
  Die
) where

import Data.Maybe
import Data.List
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
  {-# INLINE mkSimpleDie #-}
  mkSimpleDie = Die' Nothing

instance Die (Die' Int) where
  type Carried (Die' Int) = Int
  possible = poss'
  actions = act'
  {-# INLINE mkSimpleDie #-}
  mkSimpleDie = Die' Nothing

instance Die (Die' Char) where
  type Carried (Die' Char) = Char
  possible = poss'
  actions = act'
  {-# INLINE mkSimpleDie #-}
  mkSimpleDie = Die' Nothing

parseXdY :: Die d => String -> [d]
parseXdY = undefined --TODO

options :: (Die d, Die e) => d -> e -> [(Carried d, Carried e)] --This is just the cartesian product of two lists
options d e = (,) <$> possible d <*> possible e



applyOp :: (Die d, Die e, Die f) => (Carried d -> Carried e -> Carried f) -> [(Carried d, Carried e)] -> [Carried f]
applyOp f ((a,b) : xs)
  | null xs = []
  | otherwise = f a b : applyOp f xs --This can probably be simplified into a fold somehow

getStats :: Die d => [Carried d] -> [(Carried d,Int)]
getStats x = map (\l -> (head l, length l)) . group $ sort x

chanceOf :: Die d => Carried d -> [(Carried d,Int)] -> Maybe Double
chanceOf r x
  | isNothing chance = Nothing
  | otherwise = Just $ ((fst $ fromJust chance) / tot)
    where chance = find (\l -> fst l == r) x
          tot = sum $ map snd x

--Standard Dice

dZ :: Integer -> Die' Integer
dZ z = mkSimpleDie [1..z]

d4 :: Die' Integer
d4 = mkSimpleDie [1..4]

d6 :: Die' Integer
d6 = mkSimpleDie [1..6]

d8 :: Die' Integer
d8 = mkSimpleDie [1..8]

d10 :: Die' Integer
d10 = mkSimpleDie [1..10]

d12 :: Die' Integer
d12 = mkSimpleDie [1..12]

d20 :: Die' Integer
d20 = mkSimpleDie [1..20]

d100 :: Die' Integer
d100 = mkSimpleDie [1..100]

dFate :: Die' Char
dFate = mkSimpleDie ['-','-',' ', ' ','+', '+']