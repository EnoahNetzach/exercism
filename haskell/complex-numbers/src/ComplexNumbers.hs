module ComplexNumbers
(Complex,
 conjugate,
 abs,
 real,
 imaginary,
 mul,
 add,
 sub,
 div,
 complex) where

import Prelude hiding (div, abs)

-- Data definition -------------------------------------------------------------
data Complex a = Complex { real :: a, imaginary :: a } deriving(Eq, Show)

complex :: (a, a) -> Complex a
complex (a, b) = Complex { real = a, imaginary = b }

-- unary operators -------------------------------------------------------------
conjugate :: Num a => Complex a -> Complex a
conjugate c = Complex { real = real c, imaginary = -(imaginary c) }

abs :: Floating a => Complex a -> a
abs c = sqrt (real c ^ 2 + imaginary c ^ 2)

-- binary operators ------------------------------------------------------------
add :: Num a => Complex a -> Complex a -> Complex a
add a b = Complex { real = real a + real b, imaginary = imaginary a + imaginary b }

sub :: Num a => Complex a -> Complex a -> Complex a
sub a b = Complex { real = real a - real b, imaginary = imaginary a - imaginary b }

mul :: Num a => Complex a -> Complex a -> Complex a
mul Complex { real = a, imaginary = b } Complex { real = c, imaginary = d } =
  Complex { real = a * c - b * d, imaginary = b * c + a * d }

div :: Fractional a => Complex a -> Complex a -> Complex a
div Complex { real = a, imaginary = b } Complex { real = c, imaginary = d } =
  Complex { real = (a * c + b * d) / (c ^ 2 + d ^ 2), imaginary = (b * c - a * d) / (c ^ 2 + d ^ 2) }
