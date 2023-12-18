module Main where
import Point
import GHC.Arr (accum)
import Data.Char

-- whole text (split into lines), current line number
getItems:: [String] -> Double -> [(String, Point)]
getItems (l:ll) y
    | null (l:ll) = []
    | null ll = lineRes
    | otherwise = lineRes ++ getItems ll (y + 1)
    where lineRes = parseLine2 l [] (Point 0 y)

myIsSymbol :: Char -> Bool
myIsSymbol c = not (isDigit c) && (c /= '.') && (c /= '\r')

--- Doesn't work
parseLine :: String -> Int -> Int -> String -> [(String, Point)]
parseLine (c:cx) x y acc
    | null (c:cx) = []
    | c == '.' = parseLine cx (x + 1) y []
    | isDigit c = parseLine cx (x + 1) y (acc ++ [c])
    | myIsSymbol c = ([c], Point (fromIntegral x) (fromIntegral y)) : parseLine cx (x + 1) y []
    | null cx && not (null acc) = [(acc ++ [c], Point (fromIntegral x) (fromIntegral y))]
    | not (null acc) && not (isDigit c) = (acc, Point (fromIntegral (x - 1)) (fromIntegral y)) : parseLine cx (x + 1) y []

--- Doesn't work
parseLine1 :: String -> String -> Point -> [(String, Point)]
parseLine1 (l:lx) acc p
    | null (l:lx)                       = []
    | null lx       && not (null acc)   = [(acc ++ [l], p)]
    | l == '.'      && null lx          = []
    | l == '.'      && hl == '.'        = parseLine1 lx [] (pp p)
    | l == '.'      && myIsSymbol hl    = ([hl], pp p) : parseLine1 lx [] (pp p)
    | l == '.'      && isDigit hl       = parseLine1 lx [hl] (pp p)
    | myIsSymbol l  && head lx == '.'   = parseLine1 lx [] (pp p)
    | myIsSymbol l  && myIsSymbol hl    = ([hl], pp p) : parseLine1 lx [] (pp p)
    | myIsSymbol l  && isDigit hl       = parseLine1 lx [hl] (pp p)
    | isDigit l     && hl == '.'        = (acc, p) : parseLine1 lx [] (pp p)
    | isDigit l     && myIsSymbol hl    = (acc, p) : ([hl], pp p) : parseLine1 lx [] (pp p)
    | isDigit l     && isDigit hl       = parseLine1 lx (acc ++ [hl]) (pp p)
    where 
        hl = head lx
        pp p = padd p (Point 1 0)

--- Works
parseLine2 :: String -> String -> Point -> [(String, Point)]
parseLine2 s acc p
    | null s        = []
    | l == '.'      = parseLine2 lx [] (nextp p)
    | myIsSymbol l  = ([l], p) : parseLine2 lx [] (nextp p)
    | continue s    = parseLine2 lx (acc ++ [l]) (nextp p)
    | isDigit l     = (acc ++ [l], p) : parseLine2 lx [] (nextp p)
    | otherwise     = parseLine2 lx [] (nextp p)
    where 
        nextp p = padd p (Point 1 0)
        continue (s:sx) = isDigit s && not (null sx) && isDigit (head sx)
        l = head s
        lx = tail s

line1 = "..592..3..#14.."
line2 = "123"
line3 = "11...22"
line4 = "#1...22!"

digitPoints :: (String, Point) -> [Point]
digitPoints (s, p)
    | null s = []
    | otherwise = digitPoints (init s, pprev p) ++ [p]
    where
        pprev x = psub x (Point 1 0)

distPointToNearestSymbol :: Point -> [Point] -> Double
distPointToNearestSymbol p ps = minimum(map (pdist p) ps)

distNumToNearestSymbol :: (String, Point) -> [Point] -> [Double]
distNumToNearestSymbol (s, numPt) ps = map pp numPts
    where
    pp x = distPointToNearestSymbol x ps
    numPts = digitPoints (s, numPt)

isAdjacent :: (String, Point) -> [Point] -> Bool
isAdjacent (s, numPt) ps = minimum(distNumToNearestSymbol (s, numPt) ps) <= 1.5

areAdjacent :: [(String, Point)] -> [Point] -> [Bool]
areAdjacent digits pts = map (`isAdjacent` pts) digits

part1 :: [(String, Point)] -> [Point] -> Int
part1 digits symPts = sum(zipWith (\x y -> if y then read x else 0) (map fst digits) mask)
    where
    mask = areAdjacent digits symPts

main :: IO ()
main = do
    -- f <- readFile "inputs/part1.txt"
    f <- readFile "../inputs/day03.txt"
    print f
    let items = getItems(lines f) 0
    print items
    let digits = filter (isDigit . head . fst) items
    let symbols = filter (myIsSymbol . head . fst) items
    print digits
    print symbols

    -- print (map digitPoints digits)
    print(areAdjacent digits (map snd symbols))
    print(part1 digits (map snd symbols))

    -- Test parseLine2
    -- print(parseLine2 line1 [] (Point 0 1))
    -- print(parseLine2 line2 [] (Point 0 2))
    -- print(parseLine2 line3 [] (Point 0 3))
    -- print(parseLine2 line4 [] (Point 0 4))
