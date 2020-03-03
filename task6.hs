import Data.List (sort)
import Test.HUnit

median :: (Ord a, Fractional a) => [a] -> a
median x =
    if odd n
        then sort x !! (n `div` 2)
        else ((sort x !! (n `div` 2 - 1)) + (sort x !! (n `div` 2))) / 2
    where n = length x

-- HUnit Tests: 'runTestTT tests' to use them

test_median :: [Float] -> Float -> Test
test_median arg result = TestCase (assertEqual ("median " ++ show arg) result (median arg))

tests = TestList [test_median [1.0,2.0,3.0]       2.0,
                  test_median [1.0,2.0,3.0,4.0]   2.5,
                  test_median [1,2,10,11]         6.0,
                  test_median [1.0,2.0,11,10]     6.0,
                  test_median [321,9,286.0,2.0] 147.5,
                  test_median [457,321,9,286,2] 286.0]
