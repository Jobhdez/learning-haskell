data Fraction = Fraction { num :: Int, denom :: Int }

numer :: Fraction -> Int
numer (Fraction a _) = a

deno :: Fraction -> Int
deno (Fraction _ b) = b

instance Show Fraction where
  show (Fraction a b) = (show a) ++ "/" ++ (show b)

add :: Fraction -> Fraction -> Fraction
add (Fraction n m) (Fraction j k) = Fraction num denom where
  num = n*k + j*m
  denom = m*k

sub :: Fraction -> Fraction -> Fraction
sub (Fraction n m) (Fraction j k) = Fraction num denom where
  num = n*k - j*m
  denom = m*k

mul :: Fraction -> Fraction -> Fraction
mul (Fraction num denom) (Fraction num2 denom2) = Fraction numerator denominator where
  numerator = num*num2
  denominator = denom*denom2

divde :: Fraction -> Fraction -> Fraction
divde (Fraction num denom) (Fraction num2 denom2) = Fraction numerator denominator where
  numerator = num*denom2
  denominator = denom*num2
