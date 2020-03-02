import Test.HUnit

sum_proper_divisor :: Int -> Int
sum_proper_divisor x = sum [ (i) | i <- [1 .. (x - 1)], mod x i == 0 ]

proper_divisor_sequence_rec :: Int -> [Int] -> [Int]
proper_divisor_sequence_rec 1 result = 1:result
proper_divisor_sequence_rec n result
    | n `elem` result = result
    | otherwise = proper_divisor_sequence_rec (sum_proper_divisor n) (n:result)

proper_divisor_sequence :: Int -> [Int]
proper_divisor_sequence n = reverse $ proper_divisor_sequence_rec n []

-- HUnit Tests
-- 'runTestTT tests' to use them

test1 = TestCase (assertEqual "func 3,"     [3,1]                           (proper_divisor_sequence 3))
test2 = TestCase (assertEqual "func 4,"     [4,3,1]                         (proper_divisor_sequence 4))
test3 = TestCase (assertEqual "func 6,"     [6]                             (proper_divisor_sequence 6))
test4 = TestCase (assertEqual "func 10,"    [10,8,7,1]                      (proper_divisor_sequence 10))
test5 = TestCase (assertEqual "func 12,"    [12,16,15,9,4,3,1]              (proper_divisor_sequence 12))
test6 = TestCase (assertEqual "func 220,"   [220,284]                       (proper_divisor_sequence 220))
test7 = TestCase (assertEqual "func 284,"   [284,220]                       (proper_divisor_sequence 284))
test8 = TestCase (assertEqual "func 12496," [12496,14288,15472,14536,14264] (proper_divisor_sequence 12496))

tests = TestList [TestLabel "test1" test1, 
                  TestLabel "test2" test2, 
                  TestLabel "test3" test3, 
                  TestLabel "test4" test4, 
                  TestLabel "test5" test5, 
                  TestLabel "test6" test6, 
                  TestLabel "test7" test7, 
                  TestLabel "test8" test8]
