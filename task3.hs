is_palindrom :: Int -> Bool
is_palindrom n = n == (read . reverse . show $ n)

largest_palindrom :: [Int] -> Int
largest_palindrom ns = foldl max (-1) . filter is_palindrom $ ns