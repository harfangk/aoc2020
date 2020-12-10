module Day1
    ( main
    ) where

import qualified Data.Map as Map
import qualified Data.List as List

main :: IO ()
main = do
  let twoEntriesResult = findTwoEntriesWithGivenSum 2020 entries
  printResult 2020 twoEntriesResult
  let threeEntriesResult = findThreeEntriesWithGivenSum 2020 Nothing entries
  printResult 2020 threeEntriesResult

entries :: [Int]
entries = [1664, 1939, 1658, 1791, 1011, 1600, 1587, 1930, 1846, 1955, 1885, 1793, 1876, 1905, 1997, 1900, 1956, 1981, 1890, 1612, 638, 1897, 1888, 1742, 1613, 1982, 1932, 1923, 1065, 1827, 1919, 1236, 1195, 1917, 1990, 1764, 1902, 1911, 1999, 1906, 1817, 1841, 368, 747, 1881, 1941, 1894, 1898, 1887, 1958, 1862, 1940, 1819, 1873, 1959, 1977, 1301, 1945, 1961, 1673, 1879, 1889, 1872, 155, 1718, 1637, 1899, 1988, 1720, 1856, 1816, 1866, 1963, 1880, 1884, 1970, 1985, 1869, 1686, 1832, 1697, 1381, 1585, 1993, 2000, 587, 1891, 1928, 1721, 1904, 1708, 1934, 1912, 1927, 1575, 1802, 2009, 1871, 1867, 1882, 1974, 1994, 784, 1868, 1967, 1842, 1771, 2001, 1843, 1621, 1926, 1978, 2003, 1921, 1815, 1757, 2005, 1699, 1960, 2007, 1626, 1944, 2008, 1611, 2004, 1991, 1924, 1875, 1915, 1920, 1810, 1805, 1936, 1968, 882, 1976, 1874, 1987, 1826, 1910, 1483, 1964, 1855, 1979, 1996, 438, 1863, 1952, 1929, 1986, 1937, 1773, 1861, 1909, 1870, 1922, 1623, 1948, 1984, 1957, 1755, 1655, 1950, 1635, 2006, 1618, 1966, 1735, 1935, 1908, 1589, 1886, 1971, 1949, 1707, 1995, 1992, 1953, 1925, 1783, 1954, 1998, 1980, 1644, 1916, 1883, 1913, 1962, 1972, 1602, 1896, 1969, 1596, 1680, 1907, 1983, 1784, 1671, 1807, 1943]

findTwoEntriesWithGivenSum :: Int -> [Int] -> Maybe [Int]
findTwoEntriesWithGivenSum targetSum =
  snd . foldl step (Map.empty, Nothing)
  where
    step (acc, result) entry =
      case Map.lookup (targetSum - entry) acc of
        Nothing -> (Map.insert entry entry acc, result)
        Just v -> (Map.insert entry entry acc, Just [v, entry])

findThreeEntriesWithGivenSum :: Int -> Maybe [Int] -> [Int] -> Maybe [Int]
findThreeEntriesWithGivenSum _ result [] = result
findThreeEntriesWithGivenSum targetSum result (x:xs) =
  case snd . foldl step (Map.empty, Nothing) $ xs of
    Nothing -> findThreeEntriesWithGivenSum targetSum result xs
    Just entries -> Just (x:entries)
  where
    subTargetSum = 2020 - x
    step (acc, result) entry =
      case Map.lookup (subTargetSum - entry) acc of
        Nothing -> (Map.insert entry entry acc, result)
        Just v -> (Map.insert entry entry acc, Just [v, entry])

printResult :: Int -> Maybe [Int] -> IO ()
printResult targetSum Nothing = print ("Error. Couldn't find two entries that sum to " ++ show targetSum ++ ".")
printResult targetSum (Just results) = do
  print ("Two entries that sum to " ++ show targetSum ++ " are: " ++ (List.intercalate ", " . map show $ results))
  print ("Their product is: " ++ (show . product $ results))
