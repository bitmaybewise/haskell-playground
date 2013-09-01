equal a b c | a == b && b == c = 3
            | a == b = 2
            | b == c = 2
            | a == c = 2
            | otherwise = 0

media3 a b c = (a + b + c) / 3

greaterThan a b | a > b = 1
				        | otherwise = 0

greaterThanMedia a b c = greaterThan a (media3 a b c) +
						             greaterThan b (media3 a b c) +
                         greaterThan c (media3 a b c)

power2 a = a * a

power4 a = power2 (power2 a)

xor :: Bool -> Bool -> Bool -- This line is optional
xor a b = (a || b) && (not (a && b))

-- Bhaskara
delta a b c = b * b - (4 * a * c)

x operator a b c = ( (-b) `operator` (delta a b c) ) / (2 * a)
-- Another way
-- example: (+) 2 2 -> 4 || (-) 2 2 -> 0
x2 operator a b c = ( operator (-b) (delta a b c) ) / (2 * a)

xPlus  = x2 (+)
xMinus = x2 (-)
