module Output
  ( getAntisText,
  )
where

import Data.List (intercalate)
import Suspension
import Text.Printf (printf)

getAntisText :: AxleConfig -> [AxleAntis] -> String
getAntisText axlCfg antis = unlines (header : formatedAntis)
  where
    header = "Travel,Braking Anti,Acceleration Anti"
    formatedAntis = map (formatAntis axlCfg) antis

-- Bellow is the way that I want this to work
-- ------------------------------------------
-- You first generate the the text of the CSV with just the travel
-- then for each new set of data just pass that text in and
-- it should add the header and the rest of the data

formatAntis :: AxleConfig -> AxleAntis -> String
formatAntis axlCfg antis =
  intercalate "," vals
  where
    travel = printf "%.6f" (calcTravel axlCfg (axlAntisLwrArmAngle antis)) :: String
    brake = printf "%.6f" (axlAntisBraking antis) :: String
    accel = printf "%.6f" (axlAntisAcceleration antis) :: String
    vals = [travel, brake, accel]
