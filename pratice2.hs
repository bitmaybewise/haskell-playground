sumWithBound n1 n2 | n1 == n2  = n2
                   | otherwise = n1 + (sumWithBound (n1 + 1) n2)

sumWithoutBound n1 n2 = sumWithBound (n1 + 1) (n2 - 1)

multiples n1 n2 n3 | n3 == 0 = n2
                   | n1 > n2 = 0
                   | (mod n1 n3) == 0 = 1 + multiples (n1 + 1) n2 n3
                   | otherwise = multiples (n1 + 1) n2 n3

multiply _ 0 = 0
multiply 0 _ = 0
multiply a 1 = a
multiply 1 b = b
multiply a b | b < 0     = (-a) + multiply a (b + 1)
             | otherwise = a + multiply a (b - 1)

mod2 x 0 = x
mod2 0 y = 0
mod2 x y | x < y = x 
         | otherwise = mod2 (x - y) y

something 0 = 1
something x = sqrt (6 + something (x - 1))

factorial 0 = 1
factorial x = x * factorial (x - 1)

-- (m n) = m! / n!(m - n)!
newtonBinomial m n = factorial m / (factorial n * (factorial (m - n)))

-- Pell equation
pell a b inc | a >= b = pell (a - b) (b + 2) (inc + 1)
             | otherwise = inc

intSqrt x = pell x 1 0

-- Euclides algorithm
mdc a 0 = a
mdc a b = mdc b (mod2 a b)

mmc a b = a * b / mdc a b

mmc3 x y z = mmc x (mmc y z)

-- Ackerman function
acker 0 n = n + 1
acker m 0 = acker (m - 1) 1
acker m n = acker (m - 1) (acker m (n - 1))
