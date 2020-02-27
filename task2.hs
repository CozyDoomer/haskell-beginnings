is_power2_iteration :: Int -> Int -> Bool
is_power2_iteration num power2
 | power2 < num = is_power2_iteration num (power2 * 2) 
 | power2 == num = True
 | otherwise = False

is_power2 :: Int -> Bool
is_power2 x = is_power2_iteration x 1

is_reversed_power2 :: Int -> Bool
is_reversed_power2 = is_power2 . read . reverse . show
