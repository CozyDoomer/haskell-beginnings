sieve :: [Integer] -> [Integer]
sieve (x:xs) = x : sieve [y | y <- xs, mod y x > 0]

primes :: [Integer]
primes = sieve [2..]

is_prime_iter :: Integer -> [Integer] -> Bool
is_prime_iter n (x:xs)
    | x > n = False
    | x == n = True
    | otherwise = is_prime_iter n xs

is_prime :: Integer -> Bool
is_prime n = is_prime_iter n primes

is_palindrom_prime :: Integer -> Bool
is_palindrom_prime n = (is_prime . read . reverse . show $ n) && is_prime n 

double_prime :: (Integer,Integer) -> [Integer]
double_prime (n1, n2)
    | n1 < n2 = filter is_palindrom_prime [n1 .. n2]
    | otherwise = filter is_palindrom_prime [n2 .. n1]
