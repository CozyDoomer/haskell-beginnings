import Data.List (intercalate)
import Test.HUnit

sum_divisor :: Int -> Int
sum_divisor x = sum [ (i) | i <- [1 .. (x - 1)], mod x i == 0 ]

divisor_seq_rec :: Int -> [Int] -> [Int]
divisor_seq_rec 1 result = 1:result
divisor_seq_rec n result
    | n `elem` result = result
    | otherwise = divisor_seq_rec (sum_divisor n) (n:result)

divisor_seq :: Int -> [Int]
divisor_seq n = reverse $ divisor_seq_rec n []


-- HUnit Tests: 'runTestTT tests' to use them

equal_test :: Int -> [Int] -> Test
equal_test arg result = TestCase (assertEqual ("divisor_seq " ++ show arg) result (divisor_seq arg))

tests = TestList [equal_test     3 [3,1],
                  equal_test     4 [4,3,1],
                  equal_test     6 [6],
                  equal_test    10 [10,8,7,1],
                  equal_test    12 [12,16,15,9,4,3,1],
                  equal_test   220 [220,284],
                  equal_test   284 [284,220],
                  equal_test 12496 [12496,14288,15472,14536,14264]]
