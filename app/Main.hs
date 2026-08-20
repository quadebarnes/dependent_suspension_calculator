module Main (main) where

import Config
import Data.Aeson (eitherDecodeFileStrict)
import Suspension

main :: IO ()
main = do
  result <- eitherDecodeFileStrict "config.json" :: IO (Either String Config)
  case result of
    Left err -> putStrLn err
    Right config -> print (calcState config Front (calcLowerArmAngle config Front))
