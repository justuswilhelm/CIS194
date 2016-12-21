module Week6.Fibonacci where

fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 =
    map fib [0..]

fibs2 :: [Integer]
fibs2 =
    1:1:zipWith (+) fibs2 (tail fibs2)

data Stream a = Stream a (Stream a) deriving (Show, Eq)

streamToList :: Stream a -> [a]
streamToList (Stream a rest) =
    a:streamToList rest

streamRepeat :: a -> Stream a
streamRepeat a = Stream a (streamRepeat a)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap fn (Stream a rest) = Stream (fn a) (streamMap fn rest)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed fn seed =
    Stream seed $ streamFromSeed fn (fn seed)

nats :: Stream Integer
nats = streamFromSeed (+ 1) 0


natsn :: Integer -> Stream Integer
natsn = streamFromSeed (+ 1)

interleave :: Stream a -> Stream a -> Stream a
interleave (Stream a arest) b =
    Stream a (interleave b arest)
 
zeros :: Stream Integer
zeros = streamRepeat 0

ruler :: Stream Integer
ruler = interleave zeros nats
