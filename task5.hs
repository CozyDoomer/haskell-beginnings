sum_proper_divisor :: Int -> Int
sum_proper_divisor x = sum [ (i) | i <- [1 .. (x - 1)], mod x i == 0 ]

proper_divisor_sequence_rec :: Int -> [Int] -> [Int]
proper_divisor_sequence_rec 1 result = 1:result
proper_divisor_sequence_rec n result
    | n `elem` result = result
    | otherwise = proper_divisor_sequence_rec (sum_proper_divisor n) (n:result)

proper_divisor_sequence :: Int -> [Int]
proper_divisor_sequence n = reverse $ proper_divisor_sequence_rec n []

test_cases :: IO ()
test_cases = do
    print $ proper_divisor_sequence 3
    print $ proper_divisor_sequence 4
    print $ proper_divisor_sequence 6
    print $ proper_divisor_sequence 10
    print $ proper_divisor_sequence 12
    print $ proper_divisor_sequence 220
    print $ proper_divisor_sequence 284
    print $ proper_divisor_sequence 12496
    print $ length (proper_divisor_sequence 14316)