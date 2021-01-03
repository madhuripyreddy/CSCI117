module Fraction (Fraction, frac) where

-- Fraction type. ADT maintains the invariant that every fraction Frac n m
-- satisfies m > 0 and gcd n m == 1. For fractions satisfying this invariant
-- equality is the same as literal equality (hence "deriving Eq")

data Fraction = Frac Integer Integer
  deriving Eq

-- Public constructor: take two integers, n and m, and construct a fraction
-- representing n/m that satisfies the invariant, if possible
num :: Fraction -> Integer
num (Frac n _) = n

denom :: Fraction -> Integer
denom (Frac _ m) = m

frac :: Integer -> Integer -> Maybe Fraction
frac n m = frac (n `quot` factor) (m `quot` factor)
  where factor = gcd n m 
  
  
-- Show instance that outputs Frac n m as n/m
instance Show Fraction where
  show (Frac n m) = (show n) ++ " / " ++ (show m)
-- Ord instance for Fraction
instance Ord Fraction where
  compare (Frac n m) (Frac c d) = compare (n `quot` m) (c `quot` d)
  (<)  f    = (==) LT . compare f
  (>)  f    = (==) GT . compare f
  (>=) f    = not . (<) f
  (<=) f    = not . (>) f
  max  f f' = if f < f' then f' else f
  min  f f' = if f < f' then f else f'
-- Num instance for Fraction
instance Num Fraction where
  (-) f f'                  = f + (negate f')
  (+) (Frac n m) (Frac c d) = Frac num denom
    where denom = lcm m d
          num   = n * (denom `quot` m) + c * (denom `quot` d)
  (*) (Frac n m) (Frac c d) = Frac (n*c) (m*d)
  negate (Frac n m)         = Frac (-n) m
  abs f                     = fmapF abs f
    where fmapF f (Frac n m) = Frac (f n) (f m)
  fromInteger x = Frac x 1
  signum (Frac n m) = if n == 0 then 0
                        else if m > 0 then
                          if n < 0 then (-1)
                            else 1
                              else if n < 0 then 1
                              else (-1) 



