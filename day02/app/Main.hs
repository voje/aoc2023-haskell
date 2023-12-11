module Main where

import Text.Regex.TDFA
import Data.Maybe
import Data.Char

line1 = "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
line2 = "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue"
line3 = "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red"

-- Part1 logic
part1 :: String -> Int
-- part1 txt = sum(map maybeIndex (filter (not . null) (lines txt)))
part1 txt = sum(map maybeIndex (filter (not . null) (lines txt)))

part2 txt = sum(map mulMax(filter (not . null) (lines txt)))

-- Part2 line logic: multiply cube maximums
mulMax :: String -> Int
mulMax txt = maxReds txt * maxGreens txt * maxBlues txt

-- Part 1 line logic, index 0 if
-- rules don't match
maybeIndex :: String -> Int
maybeIndex txt
    | maxReds txt <= 12 && maxGreens txt <= 13 && maxBlues txt <= 14 = getIndex txt
    | otherwise = 0

-- Get Line index
getIndex :: String -> Int
getIndex txt = extractInt((txt =~ "Game [0-9]+") :: String)

extractInt :: String -> Int
extractInt x = read(filter isDigit x)

maxReds txt = maximum(getCubes "red" txt)
maxBlues txt = maximum(getCubes "blue" txt)
maxGreens txt = maximum(getCubes "green" txt)

getCubes :: String -> String -> [Int]
getCubes color txt = 
    map extractInt (getAllTextMatches(txt =~ r) :: [String])
    where
    r = "[0-9]+ " ++ color

main :: IO ()
main = do
    print(maxReds line1)
    print(getIndex line2)
    print(maybeIndex line3)
    print(mulMax line2)

    exf <- readFile "./inputs/part1.txt"
    print "example1:"
    print(part1 exf)

    f <- readFile "../inputs/day02.txt"
    print "part1:"
    print(part1 f)

    print "example2:"
    print(part2 exf)

    print "part2:"
    print(part2 f)
