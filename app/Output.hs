module Output
  ( getAntisText,
  )
where

import Data.List (intercalate)
import Geometry (radToDeg)
import Suspension
import Text.Printf (printf)

getAntisText :: AxleConfig -> [AxleAntis] -> String
getAntisText axlCfg antis = unlines (header : formatedAntis)
  where
    header = "Travel,Braking Anti,Acceleration Anti"
    formatedAntis = map (formatAntis axlCfg) antis

formatAntis :: AxleConfig -> AxleAntis -> String
formatAntis axlCfg antis =
  intercalate "," vals
  where
    travel = printf "%.6f" (calcTravel axlCfg (axlAntisLwrArmAngle antis)) :: String
    brake = printf "%.6f" (axlAntisBraking antis) :: String
    accel = printf "%.6f" (axlAntisAcceleration antis) :: String
    vals = [travel, brake, accel]
