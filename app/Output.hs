module Output
  ( splitAntis,
    getColumns,
    mergeLines,
  )
where

import Data.List (intercalate)
import Suspension
import Text.Printf (printf)

splitAntis :: [AxleAntis] -> ([Double], [Double])
splitAntis antis =
  unzip [(brake, accel) | state <- antis, let brake = axlAntisBraking state, let accel = axlAntisAcceleration state]

mergeColumns :: [String] -> [String] -> [String]
mergeColumns col1 col2 =
  map (intercalate ",") zippedVals
  where
    zippedVals = zipWith (\a b -> [a, b]) col1 col2

getColumns :: [String] -> [[Double]] -> [String]
getColumns labels vals =
  case (labels, vals) of
    ([], []) -> []
    ([label], [val]) -> getOutputColumn label val
    (hLabels : tLabels, hVals : tVals) -> mergeColumns (getOutputColumn hLabels hVals) (getColumns tLabels tVals)
    (_, _) -> error "ERROR: The number of labels does not match the number of data columns."

getOutputColumn :: String -> [Double] -> [String]
getOutputColumn label vals =
  label : map valToString vals

valToString :: Double -> String
valToString val =
  printf "%.6f" val :: String

mergeLines :: [String] -> String
mergeLines =
  unlines
