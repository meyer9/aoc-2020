import System.IO

qsort :: [Int] -> [Int]
qsort [] = []
qsort (a : xs) = qsort lower ++ [a] ++ qsort upper
  where
    lower = [x | x <- xs, x < a]
    upper = [x | x <- xs, x >= a]

checkN :: [Int] -> Int -> Int -> Maybe (Int, Int)
checkN xs target i =
  case lastNum of
    Nothing -> Nothing
    Just val -> if val + i == target then Just (val, i) else Nothing
  where
    lastNum = case takeWhile (\x -> x + i >= target) xs of
      [] -> Nothing
      xs -> Just (last xs)

firstJust :: [Maybe a] -> Maybe a
firstJust [] = Nothing
firstJust ((Just x) : _) = Just x
firstJust (_ : xs) = firstJust xs

mulMaybe :: Maybe (Int, Int, Int) -> Maybe Int
mulMaybe (Just (a, b, c)) = Just $ a * b * c
mulMaybe _ = Nothing

findTwoNumbers :: [Int] -> Int -> Maybe (Int, Int)
findTwoNumbers vals target = firstJust $ map (checkN rvals target) vals
  where
    rvals = reverse vals

findThreeNumbers :: [Int] -> Maybe (Int, Int, Int)
findThreeNumbers sortedList =
  let twoNumMap = head $ map (\i -> (i, findTwoNumbers sortedList (2020 - i))) sortedList
   in case twoNumMap of
        (i, Just (a, b)) -> Just (i, a, b)
        (i, Nothing) -> Nothing

main =
  do
    handle <- openFile "input" ReadMode
    contents <- hGetContents handle
    let out = mulMaybe $ findThreeNumbers $ qsort $ map (\line -> read line :: Int) (lines contents)
    case out of
      Just o -> print o
      Nothing -> print "Not Found!"
    hClose handle
