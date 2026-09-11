module Main (main) where

import Config
import Data.Aeson (eitherDecodeFileStrict)
import Output
import Suspension

calcAntis :: Config -> System -> ([AxleAntis], AxleConfig)
calcAntis cfg sys =
  (sweepAnti cfg axleConfig lwrArmAngle angleChange steps, axleConfig)
  where
    axleConfig = extractAxle cfg sys
    lwrArmAngle = calcLowerArmAngle axleConfig
    steps = 50
    angleChange = 0.20943951023931953

calcEverything :: Config -> String
calcEverything cfg =
  antisText
  where
    (antis, axleConfig) = calcAntis cfg Front
    antisText = getAntisText axleConfig antis

main :: IO ()
main = do
  result <- eitherDecodeFileStrict "config.json" :: IO (Either String Config)
  case result of
    Left err -> putStrLn err
    Right rawConfig -> writeFile loc (calcEverything config)
      where
        config = normalize rawConfig
        loc = "output/results.csv"
