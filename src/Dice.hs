{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Dice(
  Die
) where

import Data.List
import Data.Text(Text)
import Data.Maybe
import Data.Ratio
import Control.Applicative
import qualified Data.Text as T

class ((Eq (Carried d), Ord (Carried d), Enum (Carried d))) => Die d where
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

-- parseXdY :: Die d => Text -> [d]
-- parseXdY s
--   | elem 'd' s == False = error "no d found in string"
--   | otherwise = replicate x (dZ y) -- We need a function that converts text to digits
--     where (x,y) = breakOn (T.singleton 'd') s

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

chanceOf :: Die d => Carried d -> [(Carried d,Integer)] -> Rational
chanceOf r x
  | isNothing chance = 0 % 1
  | otherwise = snd (fromJust chance) % tot
    where chance = find (\l -> fst l == r) x
          tot = sum $ map snd x

chanceOfAtLeast :: Die d => Carried d -> [(Carried d,Integer)] -> Rational
chanceOfAtLeast r x
  | r <= minimum opts = 1 % 1
  | r > maximum opts = 0 % 1
  | isNothing idx = 0 % 1
  | otherwise = chance % tot
    where chance = sum (map snd over)
          (_,over) = splitAt (fromJust $ idx) x
          tot = sum $ map snd x
          idx = findIndex (\l -> fst l == r) x
          opts = map fst x


--Standard Dice

dZ :: Integer -> Die' Integer -- Z is used as an obtuse reference to the set of Integers
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

--N.B. The explode functions require sorted lists as inputs

--DONE: Specialise explode if explodesOn is a singleton list
--Specifically, You can use rem/div to find depth and final chance trivially
explode :: [Integer] -> Integer -> Rational
explode die target = (1 % (l ^ depth)) * (1 % remain)
                      where (depth,remain) = quotRem target (last die)
                            l = fromIntegral $ length die

--TODO: Make this more polymorphic? - needs fixing
explodeOn :: [Integer] -> [Integer] -> Integer -> Rational
explodeOn die explodesOn target
  | l == 1 = explode die target
  | target < head die = 0
  | last die > target = (1 % l) * explodeOn (map (l + ) die) explodesOn target
  | otherwise = chanceOf target (getStats die)
  where l = fromIntegral $ length die
