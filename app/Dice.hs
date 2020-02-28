{-# LANGUAGE TypeFamilies #-}
--{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Dice(
  Die
) where

class Enum (Carried d) => Die d where
  type Carried d :: *
  possible :: d -> [Carried d]
  actions :: d -> Carried d -> d
  mkSimpleDie :: Carried d -> Carried d -> d

data Die' c = Simple { poss' :: [c] } | Complex { act' :: c -> Die' c, poss' :: [c] }

instance Enum a => Die (Die' a) where
  type Carried (Die' a) = a
  possible = poss'
  actions = act'
  mkSimpleDie start end = Simple [start..end]

--mkSimpleDie :: Integer -> Integer -> Die' Integer
--mkSimpleDie start end = Simple [start..end]


--Standard Dice
d6 :: Die' Integer
d6 = mkSimpleDie 1 6

d8 :: Die' Integer
d8 = mkSimpleDie 1 8