toDigits :: Integer -> [Integer]
toDigits 0 = []
toDigits n = toDigits (n `div` 10) ++ [n `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:(y:xs)) = (x*2) : (y : doubleEveryOther xs)

sumDigit :: Integer -> Integer
sumDigit = sum . toDigits

sumDigits :: [Integer] -> Integer
sumDigits = foldr ((+) . sumDigit) 0

validate :: Integer -> Bool
validate n =
    (sumDigits . doubleEveryOther . toDigits) n `mod` 10 == 0


data Peg = One | Two | Three deriving Show
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 a b c = []
hanoi n a b c =
    hanoi (n-1) a c b ++ (a, b):hanoi  (n-1) c b a
