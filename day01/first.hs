{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Char

test1 = "1abc2\n\
\pqr3stu8vwx\n\
\a1b2c3d4e5f\n\
\treb7uchet"

fstDi :: String -> Int
fstDi a = 1

fstDiTest1 = fstDi "two1nine"

lstDi :: String -> Int
lstDi a = 0

firstLast :: String -> String
firstLast x = [head x, last x]

filterDigits :: [String] -> [Int]
filterDigits = map (read . firstLast . filter isDigit)

-- Extract first and last digit in line, sum
star1 :: String -> Int
star1 = sum . filterDigits . words

-- replOne :: T.Text -> T.Text
repl1 = T.replace "one" "1"
repl2 = T.replace "two" "2"
repl3 = T.replace "three" "3"
repl4 = T.replace "four" "4"
repl5 = T.replace "five" "5"
repl6 = T.replace "six" "6"
repl7 = T.replace "seven" "7"
repl8 = T.replace "eight" "8"
repl9 = T.replace "nine" "9"
replAll = repl9 . repl8 . repl7 . repl6 . repl5 . repl4 . repl3 . repl2 . repl1

-- Same as stare 1, words [one, two, three, ...]
-- also count as digits
star2 :: String -> Int
star2 = sum . filterDigits . words

-- Pipe the contents via stdun
main = do
    -- putStrln "-- day1 part1"
    -- inpt1 <- getContents
    -- print (star1 inpt1)
    
    print fstDiTest1

    -- putStrLn "-- day1 part2"
    -- inpt2 <- T.readFile "./inputs/example2.txt"
    -- putStrLn (T.unpack inpt2)
    -- let inpt2' = replAll inpt2
    -- let inpt2str = T.unpack inpt2'
    -- putStrLn inpt2str
    -- print (star1 inpt2str)
