module Main (main) where

import Config
import Data.Aeson (eitherDecodeFileStrict)
import Geometry
import Output
import Suspension

calcEverything :: Config -> String
calcEverything cfg =
  output
  where
    sys = Front
    axleConfig = extractAxle cfg sys
    steps = 50
    angleChange = 0.20943951023931953
    lwrArmAngle = calcRestingLwrArmAngle axleConfig

    states = sweepStates axleConfig lwrArmAngle angleChange steps

    travels = sweepTrvl cfg axleConfig states
    antis = sweepAnti cfg axleConfig states
    (brakeAntis, accelAntis) = splitAntis antis
    pinionAngles = map radToDeg (sweepHousingOrientChng axleConfig states)

    outputLines = getColumns ["Travel", "Pinion Angle Change", "Braking Anti", "Acceleration Anti"] [travels, pinionAngles, brakeAntis, accelAntis]
    output = mergeLines outputLines

main :: IO ()
main = do
  result <- eitherDecodeFileStrict "config.json" :: IO (Either String Config)
  case result of
    Left err -> putStrLn err
    Right rawConfig -> writeFile loc (calcEverything config)
      where
        config = normalize rawConfig
        loc = "output/results.csv"
