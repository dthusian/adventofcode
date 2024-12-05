import Data.List

slices :: [a] -> [[a]]
slices [] = []
slices (x:xs) = (x:xs) : slices xs

slices2 :: [[a]] -> [[[a]]]
slices2 [] = []
slices2 [x] = map (:[]) (slices x)
slices2 (x:xs) =
  let y = slices x
      ys = slices2 xs
  in zipWith (:) y ys ++ ys

matchStr :: String -> Integer
matchStr x =
  let s = take 4 x
  in (toInteger . fromEnum) (s == "XMAS" || s == "SAMX")

tileSz :: [String] -> Bool
tileSz x = length (head x) >= 4 && length x >= 4

matchH :: [String] -> Integer
matchH = matchStr . head

matchV :: [String] -> Integer
matchV x = matchStr (map head x)

matchD1 :: [String] -> Integer
matchD1 x = if tileSz x then matchStr [x !! 0 !! 0, x !! 1 !! 1, x !! 2 !! 2, x !! 3 !! 3] else 0

matchD2 :: [String] -> Integer
matchD2 x = if tileSz x then matchStr [x !! 3 !! 0, x !! 2 !! 1, x !! 1 !! 2, x !! 0 !! 3] else 0

part1 :: String -> Integer
part1 inp =
  let grid = lines inp
      s2 = slices2 grid
  in (sum . map (\x -> (sum . map (\f -> f x)) [matchH, matchV, matchD1, matchD2])) s2

part2 :: String -> Integer
part2 inp =
  let w = (length . head . lines) inp
      a1 = drop (w + 1 + 1) inp -- up left 1
      a2 = drop (w - 1 + 1) inp -- up right 1
      a3 = replicate (w - 1 + 1) '0' ++ inp -- down left 1
      a4 = replicate (w + 1 + 1) '0' ++ inp -- down right 1
      z = zipWith5 (\a b c d e -> [a, b, c, d, e]) a1 a4 a2 a3 inp
  in (toInteger . length . filter (`elem` ["MSMSA", "MSSMA", "SMMSA", "SMSMA"])) z

main :: IO ()
main = do
  contents <- readFile "input.txt"
  print (part1 contents)
  print (part2 contents)