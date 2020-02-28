import Data.Numbers.Primes

is_palindrom_prime :: Int -> Bool
is_palindrom_prime n = (isPrime . read . reverse . show $ n) &&
                 isPrime n 

double_prime :: (Int,Int) -> [Int]
double_prime (n1, n2)
    | n1 < n2 = filter is_palindrom_prime [n1 .. n2]
    | otherwise = filter is_palindrom_prime [n2 .. n1]
