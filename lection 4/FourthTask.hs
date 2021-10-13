import Data.List

fibonachi:: (Int, Int) -> Maybe (Int, (Int, Int))
fibonachi (a,b) = Just (a, (b, a + b))

fourthTask:: [Int]
fourthTask = unfoldr fibonachi (0, 1)

ft n = take n fourthTask 