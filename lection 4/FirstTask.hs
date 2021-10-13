firstTaskHelper:: Int -> [Int] -> [Int]
firstTaskHelper 0 [] = (0:[])
firstTaskHelper n x = (n:(firstTaskHelper (pred n) x))

firstTask:: Int -> [Int]
firstTask n = firstTaskHelper (pred n) []