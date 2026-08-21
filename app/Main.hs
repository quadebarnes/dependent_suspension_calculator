module Main (main) where

import Config
import Data.Aeson (eitherDecodeFileStrict)
import Suspension

main :: IO ()
main = do
  result <- eitherDecodeFileStrict "config.json" :: IO (Either String Config)
  case result of
    Left err -> putStrLn err
    Right rawConfig -> print (state) -- print (calcInstantCenter config Rear state)
      where
        config = normalize rawConfig
        lwrArmAngle = calcLowerArmAngle config Rear
        state = calcState config Rear (rearUpperArmAxleMountLoc config) lwrArmAngle
