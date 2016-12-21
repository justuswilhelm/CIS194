module Week3.Golf where

import qualified Data.Map

skips :: [a] -> [[a]]
skips [] = []
skips (x:xs) =
    (x:xs):skips xs

localMaxima :: [Integer] -> [Integer]
localMaxima (a:(b:(c:d))) =
    let rest = localMaxima (b:(c:rest))
    in if b > a && b > c then
        b:rest
    else
        rest
localMaxima _ = []

type Histogram = Data.Map.Map Integer Integer

buildBin :: [Integer] -> Histogram
buildBin [] =
    Data.Map.empty
buildBin (x:xs) =
    Data.Map.insertWith (+) x 1 $ buildBin xs

histogram :: [Integer] -> String
histogram list =
    let
        bins = buildBin list
        mostFrequent = maximum bins
        get i s =
            if Data.Map.findWithDefault 0 s bins <= i then
                " "
            else
                "*"
    in
        unlines [concatMap (get i) [0..9]| i <- reverse [0..mostFrequent-1]]
        ++ "==========\n0123456789\n"
