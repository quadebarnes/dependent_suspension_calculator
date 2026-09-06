module Main (main) where

import Config
import Data.Aeson (eitherDecodeFileStrict)
import Output
import Suspension

main :: IO ()
main = do
  result <- eitherDecodeFileStrict "config.json" :: IO (Either String Config)
  case result of
    Left err -> putStrLn err
    Right rawConfig -> writeFile loc (getAntisText axleConfig antis)
      where
        config = normalize rawConfig
        axleConfig = extractAxle config Rear
        lwrArmAngle = calcLowerArmAngle axleConfig
        -- previousUpperArmMountPos = upperArmAxleMountLoc axleConfig
        -- state = calcState axleConfig previousUpperArmMountPos lwrArmAngle
        -- ic = calcInstantCenter axleConfig state
        -- anti = calcAnti config axleConfig state
        -- states = sweepStates axleConfig lwrArmAngle 0.20943951023931953 50
        -- axleCenters = map (calcAxleCenter config axleConfig) states
        antis = sweepAnti config axleConfig lwrArmAngle 0.20943951023931953 50
        loc = "output/results.csv"
