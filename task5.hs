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

test_divisor_seq :: Int -> [Int] -> Test
test_divisor_seq arg result = TestCase (assertEqual ("divisor_seq " ++ show arg) result (divisor_seq arg))

tests = TestList [test_divisor_seq     3 [3,1],
                  test_divisor_seq     4 [4,3,1],
                  test_divisor_seq     6 [6],
                  test_divisor_seq    10 [10,8,7,1],
                  test_divisor_seq    12 [12,16,15,9,4,3,1],
                  test_divisor_seq   220 [220,284],
                  test_divisor_seq   284 [284,220],
                  test_divisor_seq 12496 [12496,14288,15472,14536,14264]]
