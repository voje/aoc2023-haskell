-- Had to remove a few trailing lines from input file, worked at last

module Main where
import Data.Char

{-
two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen
-}

test11 = "xtwone3four"
test12 = "two1nine"
test13 = "one1eightwothree2one"

test2 = "two1nine\n\
\eightwothree\n\
\abcone2threexyz\n\
\xtwone3four\n\
\4nineeightseven2\n\
\zoneight234\n\
\7pqrstsixteen"

getn (x:xs) n
    | n == 0 = []
    | n == 1 = [x]
    | null xs = [x]
    | otherwise = x : getn xs (n - 1)

txtDigit :: String -> String
txtDigit x
    | getn x 3 == "one" = "1"
    | getn x 3 == "two" = "2"
    | getn x 5 == "three" = "3"
    | getn x 4 == "four" = "4"
    | getn x 4 == "five" = "5"
    | getn x 3 == "six" = "6"
    | getn x 5 == "seven" = "7"
    | getn x 5 == "eight" = "8"
    | getn x 4 == "nine" = "9"
    | otherwise = ""

--- parse line, return string of digits
nums :: String -> String
nums (x:xs)
    | isDigit x && null xs = [x]
    | isDigit x = x : nums xs
    | null xs = []
    | not(null(txtDigit (x:xs))) = txtDigit (x:xs) ++ nums xs
    | otherwise = nums xs

-- parse string of digits, return Int representation of
-- first and last digit ("2345" -> 25)
flToInt :: String -> Int
flToInt x = read(head x : [last x])

fn01 text = sum (map (flToInt . nums) (lines text))

main :: IO ()
main = do
    print(length test13)
    print test13
    print(nums test13)
    print(flToInt $ nums test13)
    print(length(nums test13))
    print(fn01 test2)
    print "---"
    f <- readFile "inputs/star1.txt"
    print(fn01 f)

