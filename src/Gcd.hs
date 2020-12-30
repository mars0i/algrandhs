module Gcd where

-- Useful discussion of div, quot, mod, rem:
-- https://ebzzry.io/en/haskell-division/

-- | Euclid's gcd algorithm, from McEliece _Finite Fields for Computer 
-- Scientists and Engineers_, pp. 7f
euclid ri_2 ri_1 =
  let (qi, ri_0) = quotRem ri_2 ri_1
  in if ri_0 == 0
        then ri_1
        else euclid ri_1 ri_0
