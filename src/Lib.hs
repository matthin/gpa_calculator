{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Lib
    ( someFunc
    ) where

import           Data.Yaml
import           GHC.Generics
import           Text.Printf

data Activity = Activity { title :: String, percentage :: Int } deriving Generic
instance FromJSON Activity where

data Section = Section { title :: String, weight :: Int, activities :: [Activity] } deriving Generic
instance FromJSON Section where

data Course = Course { title :: String, sections :: [Section] } deriving Generic
instance FromJSON Course where

someFunc :: IO ()
someFunc = do
  courses <- decodeFileEither "example/courses.yml" :: IO (Either ParseException [Course])
  case courses of
    Left  err     -> print err
    Right courses -> case calcGpa courses of
                       Just gpa -> printf "%.1f\n" gpa
                       Nothing  -> print "percentage could not be converted to gpa"

calcPercentage :: [Course] -> Int
calcPercentage courses = calcMeanRound (map calcCoursePercentage courses)

calcGpa :: [Course] -> Maybe Float
calcGpa courses = percentageToGpa $ calcPercentage courses

calcCoursePercentage :: Course -> Int
calcCoursePercentage course = sum $ map calcSectionPercentage (sections course)

calcSectionPercentage :: Section -> Int
calcSectionPercentage section = div
  (calcMeanRound (map percentage $ activities section) * (weight section))
  100

calcMeanRound :: [Int] -> Int
calcMeanRound list = div (sum list) (length list)

percentageToGpa :: Int -> Maybe Float
percentageToGpa p
  | 90 <= p            = Just 4.0
  | 85 <= p && p <= 89 = Just 4.0
  | 80 <= p && p <= 84 = Just 3.7
  | 77 <= p && p <= 79 = Just 3.3
  | 73 <= p && p <= 76 = Just 3.0
  | 70 <= p && p <= 72 = Just 2.7
  | 67 <= p && p <= 69 = Just 2.3
  | 63 <= p && p <= 66 = Just 2.0
  | 60 <= p && p <= 62 = Just 1.7
  | 57 <= p && p <= 59 = Just 1.3
  | 53 <= p && p <= 56 = Just 1.0
  | 50 <= p && p <= 52 = Just 0.7
  | 0  <= p && p <= 49 = Just 0.0
  | otherwise = Nothing
