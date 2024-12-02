import System.IO

windows2 [] = []
windows2 (x:[]) = []
windows2 (x:xs) = (x, head xs) : windows2 xs
windows2 :: [a] -> [(a, a)]

excl1 [] = []
excl1 (x:xs) = xs : map (x :) (excl1 xs)

part1 x =
  let parsed = (map (map read . words) . lines) x
      safe l =
        let w = windows2 l
            inc = foldr ((&&) . uncurry (<)) True w
            dec = foldr ((&&) . uncurry (>)) True w
            slow = foldr ((&&) . (<= 3) . abs . uncurry (-)) True w
        in (inc || dec) && slow
  in (toInteger . length . filter safe) parsed
part1 :: String -> Integer

part2 x =
  let parsed = (map (map read . words) . lines) x
      safe l =
        let w = windows2 l
            inc = foldr ((&&) . uncurry (<)) True w
            dec = foldr ((&&) . uncurry (>)) True w
            slow = foldr ((&&) . (<= 3) . abs . uncurry (-)) True w
        in (inc || dec) && slow
  in (toInteger . length . filter (foldr ((||) . safe) False) . map excl1) parsed
part2 :: String -> Integer

main = do
  file <- openFile "input.txt" ReadMode
  contents <- hGetContents file
  print (part1 contents)
  print (part2 contents)
  hClose file