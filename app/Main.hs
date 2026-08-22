module Main (main) where

import Config
import Data.Aeson (eitherDecodeFileStrict)
import Suspension

main :: IO ()
main = do
  result <- eitherDecodeFileStrict "config.json" :: IO (Either String Config)
  case result of
    Left err -> putStrLn err
    Right rawConfig -> print anti
      where
        config = normalize rawConfig
        axleConfig = extractAxle config Rear
        lwrArmAngle = calcLowerArmAngle axleConfig
        previousUpperArmMountPos = upperArmAxleMountLoc axleConfig
        state = calcState axleConfig previousUpperArmMountPos lwrArmAngle
        ic = calcInstantCenter axleConfig state
        anti = calcAnti config axleConfig state
