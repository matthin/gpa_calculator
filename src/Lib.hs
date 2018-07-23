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
    Right courses -> printf "%d\n" (calcGpa courses)

calcGpa :: [Course] -> Int
calcGpa courses = calcMeanRound (map calcCoursePercentage courses)

calcCoursePercentage :: Course -> Int
calcCoursePercentage course = sum $ map calcSectionPercentage (sections course)

calcSectionPercentage :: Section -> Int
calcSectionPercentage section = div
  (calcMeanRound (map percentage $ activities section) * (weight section))
  100

calcMeanRound :: [Int] -> Int
calcMeanRound list = div (sum list) (length list)
