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
    steps = 50
    angleChange = 0.20943951023931953

    frontStates = sweepStates frontAxleConfig frontArmAngle angleChange steps
    frontAxleConfig = extractAxle cfg Front
    frontTravels = sweepTrvl cfg frontAxleConfig frontStates
    frontArmAngle = calcRestingLwrArmAngle frontAxleConfig
    frontAntis = sweepAnti cfg frontAxleConfig frontStates
    (antiDive, antiLift) = splitAntis frontAntis
    frontPinionAnglChng = map radToDeg (sweepHousingOrientChng frontAxleConfig frontStates)

    rearStates = sweepStates rearAxleConfig rearArmAngle angleChange steps
    rearAxleConfig = extractAxle cfg Rear
    rearTravels = sweepTrvl cfg rearAxleConfig rearStates
    rearArmAngle = calcRestingLwrArmAngle rearAxleConfig
    rearAntis = sweepAnti cfg rearAxleConfig rearStates
    (antiRise, antiSquat) = splitAntis rearAntis
    rearPinionAnglChng = map radToDeg (sweepHousingOrientChng rearAxleConfig rearStates)

    outputLines = getColumns ["Front Travel", "Rear Travel", "Front Pinion Angle Change", "Rear Pinion Angle Change", "Anti-squat", "Anti-rise", "Anti-lift", "Anti-dive"] [frontTravels, rearTravels, frontPinionAnglChng, rearPinionAnglChng, antiSquat, antiRise, antiLift, antiDive]
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
