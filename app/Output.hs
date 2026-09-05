module Output
  ( getAntisText,
  )
where

import Data.List (intercalate)
import Geometry (radToDeg)
import Suspension
import Text.Printf (printf)

getAntisText :: System -> [AxleAntis] -> String
getAntisText sys antis = unlines (header : formatedAntis)
  where
    header = "Lower Arm Angle, Braking Anti, Acceleration Anti"
    formatedAntis = map formatAntis antis

formatAntis :: AxleAntis -> String
formatAntis antis =
  intercalate "," vals
  where
    armAngle = printf "%.6f" (radToDeg (axlAntisLwrArmAngle antis)) :: String
    brake = printf "%.6f" (axlAntisBraking antis) :: String
    accel = printf "%.6f" (axlAntisAcceleration antis) :: String
    vals = [armAngle, brake, accel]
