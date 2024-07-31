module Fraction (Fraction, add, sub, mul, divide,hcf) where

type Fraction = (Int, Int)

-- Implement the `add` Function

add :: Fraction -> Fraction -> Fraction
add (n1,d1) (n2,d2) =
   let 
      numerator = (n1*d2) + (d1*n2)
      denominator = (d1*d2)      
   in simplify (numerator,  denominator)
-- Implement the `sub` Function

sub :: Fraction -> Fraction -> Fraction
sub (n1,d1) (n2,d2) = 
  let
    numerator = (n1*d2) - (d1*n2)
    denominator = (d1*d2)
    
   in simplify(numerator,denominator)
  
 
-- Implement the `mul` Function

mul :: Fraction -> Fraction -> Fraction
mul (n1,d1) (n2,d2) = 
  let 
    numerator = (n1*n2)
    denominator = (d1*d2)
    
  in simplify(numerator,denominator)

-- Implement the `divide` Function

divide :: Fraction -> Fraction -> Fraction
divide (n1,d1) (n2,d2) = 
  let 
    numerator = (n1*d2)
    denominator = (d1*n2)
    
  in simplify(numerator,denominator)

-- Implement the `hcf` Function

hcf :: Int -> Int -> Int
hcf a b 
  | b == 0    = abs a   -- Si b es 0, el HCF es el valor absoluto de a
  | a == 0    = abs b   -- Si a es 0, el HCF es el valor absoluto de b
  | otherwise = hcf b (a `mod` b)  -- Algoritmo de Euclides para otros casos



simplify:: Fraction -> Fraction
simplify (numerator , denominator) = (numerator `div` h,  denominator `div` h)
    where h = gcd numerator denominator
    