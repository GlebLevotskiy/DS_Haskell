reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]

secondTaskHelper:: Int -> [Int]
secondTaskHelper 0 = []
secondTaskHelper n = ((mod n 2):secondTaskHelper (div n 2))

secondTask:: Int -> [Int]
secondTask n = reverseList (secondTaskHelper n)