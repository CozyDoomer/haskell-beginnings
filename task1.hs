delete_iteration :: String -> Int -> Char -> Int -> String
delete_iteration [] nth c i = []
delete_iteration (first:rest) nth c i
    | first == c && i == 1 = delete_iteration rest nth c nth
    | first == c = first:delete_iteration rest nth c (i-1)
    | otherwise = first:delete_iteration rest nth c i

delete_chars :: String -> Int -> Char -> String
delete_chars s i c 
    | i <= 0 = s
    | otherwise = delete_iteration s i c i
