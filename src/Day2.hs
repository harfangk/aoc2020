module Day2
  ( main,
  )
where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Maybe as Maybe
import System.IO
import qualified Data.Array as Array

main :: IO ()
main = do
  pwCandidates <- parseData "./src/day2.txt"
  let firstInterpretationResult = length . filter (isValid CharCount) $ pwCandidates
  let secondInterpretationResult = length . filter (isValid CharPositions) $ pwCandidates
  print ("The number of valid passwords according to the first policy interpretation is: " ++ show firstInterpretationResult)
  print ("The number of valid passwords according to the second policy interpretation is: " ++ show secondInterpretationResult)

data Policy = Policy {n1 :: Int, n2 :: Int, char :: Char}
  deriving (Show)

data PwCandidate = PwCandidate {policy :: Policy, password :: String}
  deriving (Show)

data PolicyInterpretationRule = CharCount | CharPositions

parseData :: FilePath -> IO [PwCandidate]
parseData fp = withFile fp ReadMode $ \h -> do
  getCase [] h
  where
    getCase acc h = do
      isEOF <- hIsEOF h
      if isEOF
        then pure acc
        else do
          parsedResult <- parseLine <$> BS.hGetLine h
          getCase (parsedResult : acc) h
    parseLine :: BS.ByteString -> PwCandidate
    parseLine line = Maybe.fromJust $ do
      (min, line') <- BS.readInt line
      (max, line'') <- BS.readInt (BS.drop 1 line')
      (char, line''') <- BS.uncons (BS.drop 1 line'')
      let password = BS.unpack . BS.dropWhile (\x -> (x == ' ') || (x == ':')) $ line'''
      pure (PwCandidate (Policy min max char) password)

isValid :: PolicyInterpretationRule -> PwCandidate -> Bool
isValid rule c =
  applyPolicy rule (policy c) (password c)
  where
    applyPolicy :: PolicyInterpretationRule -> Policy -> String -> Bool
    applyPolicy CharCount policy password =
      (charCount >= n1 policy) && (charCount <= n2 policy)
      where
        charCount = length . filter (== char policy) $ password

    applyPolicy CharPositions Policy{n1=n1, n2=n2, char=char} password =
      ((pwArray Array.! n1 == char) && (pwArray Array.! n2 /= char)) ||
      ((pwArray Array.! n1 /= char) && (pwArray Array.! n2 == char))
      where
        pwArray = Array.listArray (1, length password) password
