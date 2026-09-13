module Output
  ( splitAntis,
    mergeOutputColumns,
    mergeLines,
  )
where

import Data.List (intercalate)
import Suspension
import Text.Printf (printf)

splitAntis :: [AxleAntis] -> ([Double], [Double])
splitAntis antis =
  unzip [(brake, accel) | state <- antis, let brake = axlAntisBraking state, let accel = axlAntisAcceleration state]

mergeOutputColumns :: [String] -> String -> [Double] -> [String]
mergeOutputColumns prevColumns label vals =
  case prevColumns of
    [] -> column
    _ -> map (intercalate ",") zippedVals
  where
    column = getOutputColumn label vals
    zippedVals = zipWith (\a b -> [a, b]) prevColumns column

getOutputColumn :: String -> [Double] -> [String]
getOutputColumn label vals =
  label : map valToString vals

valToString :: Double -> String
valToString val =
  printf "%.6f" val :: String

mergeLines :: [String] -> String
mergeLines =
  unlines
