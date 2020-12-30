module Day3
  ( main,
  )
where

import Data.Array as Array

main :: IO ()
main = do
  field <- parseData "./src/day3.txt"
  let slopes = [(1,1), (3,1), (5,1), (7,1), (1,2)]
      treeCounts = map (countTreeEncounter field) slopes
      resultInput = zip slopes treeCounts
  printResults resultInput
  print ("The product of the number of encountered trees is: " ++ (show . product $ treeCounts))


parseData :: FilePath -> IO (Array (Int, Int) Char)
parseData fp = do
  field <- words <$> readFile fp
  let xSize = length . head $ field
  let ySize = length field
  return (Array.listArray ((0,0),(ySize - 1, xSize - 1)) . concat $ field)

countTreeEncounter :: Array (Int, Int) Char -> (Int, Int) -> Int
countTreeEncounter array (slopeX, slopeY) =
  traverse (0,0) 0
  where
    yBound = (fst . snd . Array.bounds $ array) + 1
    xBound = (snd . snd . Array.bounds $ array) + 1
    traverse (y,x) count =
      if y >= yBound then
        count
      else
        traverse (y', x') count'
      where
        count' =
          if array Array.! (y,x) == '#' then
            count + 1
          else
            count
        y' = y + slopeY
        x' = rem (x + slopeX) xBound

printResults :: [((Int,Int), Int)] -> IO ()
printResults results = do
  mapM_ f results
  where
    f ((slopeX, slopeY), result) = print ("The number of trees encountered during a toboggan ride down the slope of (" ++ show slopeX ++ ", " ++ show slopeY ++ ") is: " ++ show result)
