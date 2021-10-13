isqrt:: Integral a => a -> a
isqrt = floor . sqrt . fromIntegral

isPrimeHelper:: Int -> Int -> Bool
isPrimeHelper n 1 = True
isPrimeHelper n i = 
	if (mod n i) == 0
		then False
		else isPrimeHelper n (pred i)

isPrime:: Int -> Bool
isPrime n = isPrimeHelper n (isqrt n)

factors n = [x | x <- [2..n], mod n x == 0]

primes:: Int -> [Int]
primes n = filter (\n -> isPrime n) (factors n)
