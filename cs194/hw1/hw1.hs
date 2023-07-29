-- Ex 1
toDigits :: Integer -> [Integer]
toDigits 0 = []
toDigits n = if n < 0 then [] else toDigits (n `div` 10) ++ [n `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev n = reverse (toDigits n)

-- Ex 2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther n = [if odd (length n - i) then x else x * 2 | (i, x) <- zip [0..] n]

-- Ex 3
digitList = map (sum . toDigits)

sumDigits :: [Integer] -> Integer
sumDigits n = sum (digitList n)

-- Ex 4
validate :: Integer -> Bool
validate n = sumDigits (doubleEveryOther (toDigits n)) `mod` 10 == 0