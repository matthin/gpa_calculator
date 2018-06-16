{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}

module Lib
    ( someFunc
    ) where

import Text.Printf
import GHC.Generics
import Data.Yaml

strToGradePoint :: String -> Maybe Float
strToGradePoint "A+" = Just 4.0
strToGradePoint "A"  = Just 4.0
strToGradePoint "A-" = Just 3.7
strToGradePoint "B+" = Just 3.3
strToGradePoint "B"  = Just 3.0
strToGradePoint "B-" = Just 2.7
strToGradePoint "C+" = Just 2.3
strToGradePoint "C"  = Just 2.0
strToGradePoint "C-" = Just 1.7
strToGradePoint "D+" = Just 1.3
strToGradePoint "D"  = Just 1.0
strToGradePoint "D-" = Just 0.7
strToGradePoint "F"  = Just 0.0
strToGradePoint _    = Nothing

data Course = Course { title :: String, grade :: String } deriving Generic
instance FromJSON Course where

someFunc :: IO ()
someFunc = (decodeFile "example/courses.yml" :: IO (Maybe [Course])) >>= \case
              Just courses -> case calcGpa (map grade courses) of
                                Just gpa -> printf "%.2f\n" gpa
                                Nothing -> putStrLn "Invalid letter grade in courses.yml"
              Nothing -> putStrLn "courses.yml could not be decoded"

calcGpa :: [String] -> Maybe Float
calcGpa grades = case fmap sum . sequence $ map strToGradePoint grades of
                   Just total -> Just (total / fromIntegral (length grades))
                   Nothing -> Nothing
