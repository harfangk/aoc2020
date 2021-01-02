{-# LANGUAGE FlexibleContexts #-}

module Day4
  ( main,
  )
where

import qualified Data.List.Split as Split
import qualified Data.List as List
import qualified Data.Char as Char
import qualified Data.Either as Either
import qualified Data.Maybe as Maybe
import Text.Parsec
import Text.Parsec.Perm (permute, (<$?>), (<|?>))

main :: IO ()
main = do
  entries <- parseData "./src/day4.txt"
  let passports = Maybe.mapMaybe entryToPassport entries
  let validPassports = Maybe.mapMaybe validatePassport passports
  print validPassports
  print ("The number of passports with required fields is: " ++ show (length passports))
  print ("The number of valid passports with required fields is: " ++ show (length validPassports))

parseData :: FilePath -> IO [Entry]
parseData fp =
  Either.rights . map (parse entryParser fp . unlines . words) . Split.splitOn "\n\n" <$> readFile fp

data Entry = Entry
  { byr :: Maybe Byr,
    iyr :: Maybe Iyr,
    eyr :: Maybe Eyr,
    hgt :: Maybe Hgt,
    hcl :: Maybe Hcl,
    ecl :: Maybe Ecl,
    pid :: Maybe Pid,
    cid :: Maybe Cid
  }
  deriving (Show)

data Passport = Passport Byr Iyr Eyr Hgt Hcl Ecl Pid deriving Show

newtype Byr = Byr String deriving (Show)
newtype Iyr = Iyr String deriving (Show)
newtype Eyr = Eyr String deriving (Show)
newtype Hgt = Hgt String deriving (Show)
newtype Hcl = Hcl String deriving (Show)
newtype Ecl = Ecl String deriving (Show)
newtype Pid = Pid String deriving (Show)
newtype Cid = Cid String deriving (Show)

data ValidPassport = ValidPassport ValidByr ValidIyr ValidEyr ValidHgt ValidHcl ValidEcl ValidPid deriving Show

newtype ValidByr = ValidByr Int deriving (Show)
newtype ValidIyr = ValidIyr Int deriving (Show)
newtype ValidEyr = ValidEyr Int deriving (Show)
data ValidHgt = Cm Int
              | In Int
              deriving (Show)
newtype ValidHcl = ValidHcl String deriving (Show)
newtype ValidEcl = ValidEcl Color deriving (Show)
newtype ValidPid = ValidPid String deriving (Show)

data Color = Amb | Blu | Brn | Gry | Grn | Hzl | Oth
  deriving Show

entryToPassport :: Entry -> Maybe Passport
entryToPassport e = Just Passport <*> byr e <*> iyr e <*> eyr e <*> hgt e <*> hcl e <*> ecl e <*> pid e

validatePassport :: Passport -> Maybe ValidPassport
validatePassport (Passport byr iyr eyr hgt hcl ecl pid) =
  Just ValidPassport <*> validateByr byr <*> validateIyr iyr <*> validateEyr eyr <*> validateHgt hgt <*> validateHcl hcl <*> validateEcl ecl <*> validatePid pid
  where
    validateByr (Byr byr) =
      if intValue >= 1920 && intValue <= 2002 then
        Just (ValidByr intValue)
      else
        Nothing
      where
        intValue = read byr :: Int
    validateIyr (Iyr iyr) =
      if intValue >= 2010 && intValue <= 2020 then
        Just (ValidIyr intValue)
      else
        Nothing
      where
        intValue = read iyr :: Int
    validateEyr (Eyr eyr) =
      if intValue >= 2020 && intValue <= 2030 then
        Just (ValidEyr intValue)
      else
        Nothing
      where
        intValue = read eyr :: Int
    validateHgt (Hgt hgt)
      | unit == "cm" && number' >= 150 && number' <= 193 = Just (Cm number')
      | unit == "in" && number' >= 59 && number' <= 76 = Just (In number')
      | otherwise = Nothing
      where
        (number, unit) = List.splitAt (length hgt - 2) hgt
        number' = read number :: Int
    validateHcl (Hcl hcl) =
      case result of
        Right value -> Just (ValidHcl value)
        Left _ -> Nothing
      where
        parser = do
          char '#'
          value <- count 6 hexDigit
          eof
          return ('#':value)
        result = parse parser "" hcl
    validateEcl (Ecl ecl) =
      case ecl of
        "amb" -> Just (ValidEcl Amb)
        "blu" -> Just (ValidEcl Blu)
        "brn" -> Just (ValidEcl Brn)
        "gry" -> Just (ValidEcl Gry)
        "grn" -> Just (ValidEcl Grn)
        "hzl" -> Just (ValidEcl Hzl)
        "oth" -> Just (ValidEcl Oth)
        _ -> Nothing
    validatePid (Pid pid) =
      if length pid == 9 && List.all Char.isDigit pid then
        Just (ValidPid pid)
      else
        Nothing

entryParser = do
  (byr, iyr, eyr, hgt, hcl, ecl, pid, cid) <-
    permute
      ( (,,,,,,,)
          <$?> attr "byr"
          <|?> attr "iyr"
          <|?> attr "eyr"
          <|?> attr "hgt"
          <|?> attr "hcl"
          <|?> attr "ecl"
          <|?> attr "pid"
          <|?> attr "cid"
      )
  return $ Entry (fmap Byr byr) (fmap Iyr iyr) (fmap Eyr eyr) (fmap Hgt hgt) (fmap Hcl hcl) (fmap Ecl ecl) (fmap Pid pid) (fmap Cid cid)
  where
    attr txt = (Nothing, fmap Just (try $ attributeParser txt))
    attributeParser field = do
      string $ field ++ ":"
      value <- many1 (noneOf "\n")
      endOfLine
      return value
