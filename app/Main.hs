--  OVERALL PLAN
-- 1. Load in the .json file with the positions of each joint
-- 2. Calculate the length and angle of the lower arm
-- 3. Generate a list of lower arm anagle that the characterisitics should be generated at from full
--      compression (bumps stop angle) to full drop (limit strap angle) which should be inputs from the user.
-- 4. run each angle through the calculator and store the results in a new list of outputs
-- 5. save each output to a csv file that can be looked at later.
-- create a struct that represnets a setup of a vehicle
-- Load the json file into that struct
-- caculate the length and angle of the front and rear lower arms. Save those into new
--      "state" struct.
-- create a function that genreates states from the base state and returns a list of all
--      the states
-- generate the characterisitcs from each state
--      generate the position of the axle housing based on the input state
--      used the generated position to cacl IC anti roll and pinion angle
-- save the results to a csv file
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Data.Aeson (FromJSON, Result (Success), eitherDecodeFileStrict)
import GHC.Generics (Generic)

data Point = Point
  { x :: Double,
    y :: Double,
    z :: Double
  }
  deriving (Show, Generic)

instance FromJSON Point

data Config = Config
  { wheelbase :: Double,
    brakeBias :: Double,
    driveBias :: Double,
    mass :: Double,
    massSprung :: Double,
    massUnsprung :: Double,
    wheelRollingRadius :: Double,
    frontShockAxleMountingLoc :: Point,
    frontShockFrameMountingLoc :: Point,
    frontShockExtendedLength :: Double,
    frontShockCompressedLength :: Double,
    rearShockAxleMountingLoc :: Point,
    rearShockFrameMountingLoc :: Point,
    rearShockExtendedLength :: Double,
    rearShockCompressedLength :: Double,
    frontUpperArmFrameMountLoc :: Point,
    frontUpperArmAxleMountLoc :: Point,
    frontLowerArmFrameMountLoc :: Point,
    frontLowerArmAxleMountLoc :: Point,
    frontTrackBarAxleMountLoc :: Point,
    frontTrackBarFrameMountLoc :: Point,
    rearUpperArmFrameMountLoc :: Point,
    rearUpperArmAxleMountLoc :: Point,
    rearLowerArmFrameMountLoc :: Point,
    rearLowerArmAxleMountLoc :: Point
  }
  deriving (Show, Generic)

instance FromJSON Config

calc3dDistance :: Point -> Point -> Double
calc3dDistance p1 p2 = sqrt (dx ^ 2 + dy ^ 2 + dz ^ 2)
  where
    dx = x p2 - x p1
    dy = y p2 - y p1
    dz = z p2 - z p1

calcFrontLowerArmLength :: Config -> Double
calcFrontLowerArmLength cfg = calc3dDistance (frontLowerArmFrameMountLoc cfg) (frontLowerArmAxleMountLoc cfg)

calcRearLowerArmLength :: Config -> Double
calcRearLowerArmLength cfg = calc3dDistance (rearLowerArmAxleMountLoc cfg) (rearLowerArmFrameMountLoc cfg)

radToDeg :: Double -> Double
radToDeg rads = rads * (180 / pi)

calcXZAngle :: Point -> Point -> Double
calcXZAngle p1 p2 = atan2 rise run
  where
    rise = z p2 - z p1
    run = x p2 - x p1

calcFrontLowerArmAngle :: Config -> Double
calcFrontLowerArmAngle cfg = calcXZAngle (frontLowerArmAxleMountLoc cfg) (frontLowerArmFrameMountLoc cfg)

calcRearLowerArmAngle :: Config -> Double
calcRearLowerArmAngle cfg = calcXZAngle (rearLowerArmAxleMountLoc cfg) (rearLowerArmFrameMountLoc cfg)

main :: IO ()
main = do
  result <- eitherDecodeFileStrict "config.json" :: IO (Either String Config)
  case result of
    Left err -> putStrLn err
    Right config -> print (radToDeg (calcFrontLowerArmAngle config))
