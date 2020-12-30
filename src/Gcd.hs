module Gcd where

import Debug.Trace (trace)


-- Useful discussion of div, quot, mod, rem:
-- https://ebzzry.io/en/haskell-division/


-- | Euclid's gcd algorithm, from 
-- McEliece _Finite Fields for Computer -- Scientists and Engineers_, pp. 7f.
-- ri2 means r_{i-2}, etc.
euclid :: Integral t => t -> t -> t
euclid ri2 ri1 =
  let (qi, ri) = quotRem ri2 ri1
  in if ri == 0
        then ri1
        else euclid ri1 ri


-- | Helper function for extended Euclid's gcd algorithm, from 
-- McEliece _Finite Fields for Computer -- Scientists and Engineers_, p. 9.
-- Given arguments a, b, 1, 0, 0, 1 returns a triple (g, s, t) where 
-- g is the GCD, and g = sa + tb.
-- Since the point of this is to show the algorithm in action, trace
-- output is built in.
extender :: (Show t, Integral t) => t -> t -> t -> t -> t -> t -> (t, t, t)
extender ri2 ri1 si2 si1 ti2 ti1 =
  let (qi, ri) = quotRem ri2 ri1
      si = si2 - qi*si1
      ti = ti2 - qi*ti1
  in if ri == 0
        then (ri1, si1, ti1)
        else trace ("qi: " ++   show qi ++
                    "\nri: " ++ show ri ++ 
                    ", ri2: " ++ show ri2 ++ 
                    ", ri1: " ++ show ri1 ++ 
                    "\nsi: " ++ show si ++ 
                    ", si2: " ++ show si2 ++ 
                    ", si1: " ++ show si1 ++ 
                    "\nti: " ++ show ti ++ 
                    ", ti2: " ++ show ti2 ++ 
                    ", ti1: " ++ show ti1 ++ "\n")
                   extender ri1 ri si1 si ti1 ti


-- | Extended Euclid's gcd algorithm, from 
-- McEliece _Finite Fields for Computer -- Scientists and Engineers_, p. 9.
-- Given arguments a, b, returns a triple (g, s, t) where g is the GCD,
-- and g = sa + tb.  (This function is really just a wrapper for extender.)
extendedEuclid :: (Show t, Integral t) => t -> t -> (t, t, t)
extendedEuclid a b = extender a b 1 0 0 1
