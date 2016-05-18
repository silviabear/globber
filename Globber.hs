module Globber (matchGlob) where

import Data.HashSet
import Data.List

type GlobPattern = String

data Pattern = Literal Char
             | Any
             | AnyMore
             | Set { set :: HashSet[Int] }
             | End

data PatternSeg = PatternSeg { pattern :: Pattern, nextPattern :: GlobPattern }
data StringSeg = StringSeg { ch :: Char, nextString :: String }

isMatch :: Pattern -> Char -> Bool
isMatch Any c = True
isMatch AnyMore c = True
isMatch (Literal ch) c
  | ch == c = True
  | otherwise = False

extractSet :: GlobPattern -> Maybe PatternSeg
extractSet pattern = case elemIndex ']' pattern of
  Just index -> Nothing
  Nothing -> Nothing
  
readString :: String -> Maybe StringSeg
readString string
  | length string == 0 = Nothing
  | otherwise = Just StringSeg { ch = head string, nextString = drop 1 string }

readPattern :: GlobPattern -> Maybe PatternSeg
readPattern pattern
  | length pattern == 0 = Just PatternSeg { pattern = End, nextPattern = "" }
  | p == '\\' = Just PatternSeg { pattern = Literal (head remain), nextPattern = drop 1 remain }
  | p == '?' = Just PatternSeg { pattern = Any, nextPattern = remain }
  | p == '*' = Just PatternSeg { pattern = AnyMore, nextPattern = remain }
  | p == '[' = extractSet remain
  | otherwise = Just PatternSeg { pattern = Literal p, nextPattern = remain }
  where p = head pattern
        remain = drop 1 pattern


matchGlob :: GlobPattern -> String -> Bool
matchGlob pattern string = loop pattern string
  where loop p s = case stringSeg of
          Just (StringSeg ch nextString) -> case patternSeg' of
            Just (PatternSeg pattern nextPattern) -> case pattern of
              End -> False
              _ -> if isMatch pattern ch then loop nextPattern nextString else False
            Nothing -> False
          Nothing -> case patternSeg' of
            Just (PatternSeg pattern nextPattern) -> case pattern of
              End -> True
              AnyMore -> True
              _ -> False
            Nothing -> False
          where stringSeg = readString string
                patternSeg' = readPattern pattern
