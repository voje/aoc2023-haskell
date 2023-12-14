module Point where

-- Note wanted to make Point Num Num but don't understand.
-- Constraints enough. Double solves everything but it's costly.
data Point = Point Double Double deriving(Show, Eq)

(Point x1 y1) `padd` (Point x2 y2) = Point (x1 + x2) (y1 + y2)
(Point x1 y1) `psub` (Point x2 y2) = Point (x1 - x2) (y1 - y2)
(Point x1 y1) `pmul` (Point x2 y2) = Point (x1 * x2) (y1 * y2)

pdist (Point x1 y1) (Point x2 y2) =
    sqrt((x2 - x1)^2 + (y2 - y1)^2)
