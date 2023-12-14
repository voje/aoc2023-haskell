module Main where
import Point

-- whole text (split into lines), current line number
getNums :: [String] -> Int -> [(String, Point)]
getNums (x:xs) line
    | null (x:xs) = []
    | null xs = lineRes
    | otherwise = lineRes ++ getNums xs (line + 1)
    where lineRes = getNums' x line

-- one-line
getNums' :: String -> Int -> [(String, Point)]
getNums' s line = [("test", Point 1 (fromIntegral line))]

line1 = "..592....."

main :: IO ()
main = do
    -- f <- readFile "inputs/part1.txt"
    -- print f
    -- print (getNums (lines f) 0)
    print(getNums' line1 9)
