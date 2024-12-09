import Data.Maybe
import Data.List
import Debug.Trace

data Vec2 = Vec2 Int Int deriving (Eq, Show)

vx :: Vec2 -> Int
vx (Vec2 x _) = x

vy :: Vec2 -> Int
vy (Vec2 _ y) = y

vadd :: Vec2 -> Vec2 -> Vec2
vadd (Vec2 x1 y1) (Vec2 x2 y2) = Vec2 (x1 + x2) (y1 + y2)

vsub :: Vec2 -> Vec2 -> Vec2
vsub (Vec2 x1 y1) (Vec2 x2 y2) = Vec2 (x1 - x2) (y1 - y2)

vmul :: Vec2 -> Int -> Vec2
vmul (Vec2 x y) d = Vec2 (x * d) (y * d)

vdiv :: Vec2 -> Int -> Vec2
vdiv (Vec2 x y) d = Vec2 (x `div` d) (y `div` d)

divisible :: Vec2 -> Int -> Bool
divisible (Vec2 x y) d = mod x d == 0 && mod y d == 0

antinodes :: Vec2 -> Vec2 -> [Vec2]
antinodes a b =
  let diff = vsub b a
  in if a == b then [] else catMaybes [
    if divisible diff 3 then Just (vadd a (vdiv diff 3)) else Nothing,
    if divisible diff 3 then Just (vsub b (vdiv diff 3)) else Nothing,
    Just (vsub a diff),
    Just (vadd b diff)]

pairs :: [Vec2] -> [(Vec2, Vec2)]
pairs a = [(x, y) | x <- a, y <- a]

readGrid :: String -> Int -> Int -> Char -> [Vec2]
readGrid [] _ _ _ = []
readGrid (l:ls) x y c | l == '\n' = readGrid ls 0 (y + 1) c
                      | l == c = Vec2 x y : readGrid ls (x + 1) y c
                      | otherwise = readGrid ls (x + 1) y c

part1 :: String -> Int
part1 inp =
  let finder = readGrid inp 0 0
      chars = (delete '\n' . delete '.' . nub) inp
      w = length (head (lines inp))
      h = length (lines inp)
      inBounds (Vec2 x y) = x >= 0 && y >= 0 && x < w && y < h
  in (
    length .
    nub .
    concatMap (
      concatMap (filter inBounds . uncurry antinodes) .
      pairs .
      finder)
    ) chars

antinodes2 :: Vec2 -> Vec2 -> [Vec2]
antinodes2 a b =
  let diff = vsub b a
      freq = gcd (vx diff) (vy diff)
      fracPart = (concatMap ((\x -> [vadd a x, vsub b x]) . vdiv diff) . filter (\x -> freq `mod` x == 0)) [2..freq]
      intPart = concatMap ((\x -> [vsub a x, vadd b x]) . vmul diff) [0..50]
  in fracPart ++ intPart

part2 :: String -> Int
part2 inp =
  let finder = readGrid inp 0 0
      chars = (delete '\n' . delete '.' . nub) inp
      w = length (head (lines inp))
      h = length (lines inp)
      inBounds (Vec2 x y) = x >= 0 && y >= 0 && x < w && y < h
  in (
    length .
    nub .
    concatMap (
      concatMap (filter inBounds . uncurry antinodes2) .
      pairs .
      finder)
    ) chars

main :: IO ()
main = do
  contents <- readFile "input.txt"
  print (part1 contents)
  print (part2 contents)