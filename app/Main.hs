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

data UnitVector = UnitVector
  { uvx :: Double,
    uvy :: Double,
    uvz :: Double
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

data State = State
  { lowerArmAngle :: Double,
    upperArmAxleMountPos :: Point,
    lowerArmAxleMountPos :: Point
  }
  deriving (Show, Generic)

data System = Front | Rear

calc2dDistance :: Point -> Point -> Double
calc2dDistance p1 p2 = sqrt (dx ^ 2 + dz ^ 2)
  where
    dx = x p2 - x p1
    dz = z p2 - z p1

calc3dDistance :: Point -> Point -> Double
calc3dDistance p1 p2 = sqrt (dx ^ 2 + dy ^ 2 + dz ^ 2)
  where
    dx = x p2 - x p1
    dy = y p2 - y p1
    dz = z p2 - z p1

calcLowerArmLength :: Config -> System -> Double
calcLowerArmLength cfg sys = case sys of
  Front -> calc3dDistance (frontLowerArmFrameMountLoc cfg) (frontLowerArmAxleMountLoc cfg)
  Rear -> calc3dDistance (rearLowerArmAxleMountLoc cfg) (rearLowerArmFrameMountLoc cfg)

calcLowerArmSideLength :: Config -> System -> Double
calcLowerArmSideLength cfg sys = case sys of
  Front -> calc2dDistance (frontLowerArmFrameMountLoc cfg) (frontLowerArmAxleMountLoc cfg)
  Rear -> calc2dDistance (rearLowerArmAxleMountLoc cfg) (rearLowerArmFrameMountLoc cfg)

calcUpperArmSideLength :: Config -> System -> Double
calcUpperArmSideLength cfg sys = case sys of
  Front -> calc2dDistance (frontUpperArmFrameMountLoc cfg) (frontUpperArmAxleMountLoc cfg)
  Rear -> calc2dDistance (rearUpperArmAxleMountLoc cfg) (rearUpperArmFrameMountLoc cfg)

radToDeg :: Double -> Double
radToDeg rads = rads * (180 / pi)

calcXZAngle :: Point -> Point -> Double
calcXZAngle p1 p2 = atan2 rise run
  where
    rise = z p2 - z p1
    run = x p2 - x p1

calcLowerArmAngle :: Config -> System -> Double
calcLowerArmAngle cfg sys = case sys of
  Front -> calcXZAngle (frontLowerArmAxleMountLoc cfg) (frontLowerArmFrameMountLoc cfg)
  Rear -> calcXZAngle (rearLowerArmAxleMountLoc cfg) (rearLowerArmFrameMountLoc cfg)

-- The functions bellow are for calculating the location of the lower arm axle side mount

calcAxleMountsDistance :: Config -> System -> Double
calcAxleMountsDistance cfg sys = case sys of
  Front -> calc2dDistance (frontUpperArmAxleMountLoc cfg) (frontLowerArmAxleMountLoc cfg)
  Rear -> calc2dDistance (rearUpperArmAxleMountLoc cfg) (rearLowerArmAxleMountLoc cfg)

calcDistanceBetweenCenters :: Point -> Point -> Double
calcDistanceBetweenCenters p0 p1 = abs (calc2dDistance p1 p0)

calcDistanceToRadicalLine :: Double -> Double -> Double -> Double
calcDistanceToRadicalLine r0 r1 d = (r0 ^ 2 - r1 ^ 2 + d ^ 2) / (2 * d)

calcPerpendicularOffset :: Double -> Double -> Double
calcPerpendicularOffset r0 a = sqrt (r0 ^ 2 - a ^ 2)

-- The problems is the unit vector input here and in later usages
calcCenterLinePoint :: Point -> UnitVector -> Double -> Point
calcCenterLinePoint p0 u a =
  Point
    { x = x p0 + (a * uvx u),
      y = 0,
      z = z p0 + (a * uvz u)
    }

calcUnitVector :: Point -> Point -> UnitVector
calcUnitVector p0 p1 =
  UnitVector
    { uvx = vx / d,
      uvy = 0,
      uvz = vz / d
    }
  where
    vx = x p1 - x p0
    vz = z p1 - z p0
    d = sqrt (vx ^ 2 + vz ^ 2)

calcSteppedPerpendicular :: Point -> Double -> UnitVector -> Point
calcSteppedPerpendicular p2 h u =
  Point
    { x = x p2 + h * (-uvz u),
      y = 0,
      z = z p2 + h * uvx u
    }

-- inputs:
-- p0 -> lowerArmAxleMountLoc
calcUpperArmAxleMountLoc :: Config -> System -> Point -> Point
calcUpperArmAxleMountLoc cfg sys p0 = calcSteppedPerpendicular p2 h u
  where
    p1 = frontUpperArmFrameMountLoc cfg
    r0 = calcAxleMountsDistance cfg sys
    r1 = calcUpperArmSideLength cfg sys
    d = calcDistanceBetweenCenters p0 p1
    a = calcDistanceToRadicalLine r0 r1 d
    h = calcPerpendicularOffset r0 a
    u = calcUnitVector p0 p1
    p2 = calcCenterLinePoint p0 u a

calcState :: Config -> System -> Double -> Double -> State
calcState cfg sys armAngle armSideLength = case sys of
  Front ->
    State
      { lowerArmAngle = armAngle,
        upperArmAxleMountPos =
          Point -- This is wrong because the upper arm does not share the same angle as the lower
            { x = x (frontUpperArmFrameMountLoc cfg) - armSideLength * cos armAngle,
              y = y (frontUpperArmAxleMountLoc cfg),
              z = z (frontUpperArmFrameMountLoc cfg) - armSideLength * sin armAngle
            },
        lowerArmAxleMountPos =
          Point
            { x = x (frontLowerArmFrameMountLoc cfg) - armSideLength * cos armAngle,
              y = y (frontLowerArmAxleMountLoc cfg),
              z = z (frontLowerArmFrameMountLoc cfg) - armSideLength * sin armAngle
            }
      }
  Rear ->
    State
      { lowerArmAngle = armAngle,
        upperArmAxleMountPos =
          Point
            { x = x (rearUpperArmFrameMountLoc cfg) - armSideLength * cos armAngle,
              y = y (rearUpperArmAxleMountLoc cfg),
              z = z (rearUpperArmFrameMountLoc cfg) - armSideLength * sin armAngle
            },
        lowerArmAxleMountPos =
          Point
            { x = x (rearLowerArmFrameMountLoc cfg) - armSideLength * cos armAngle,
              y = y (rearLowerArmAxleMountLoc cfg),
              z = z (rearLowerArmFrameMountLoc cfg) - armSideLength * sin armAngle
            }
      }

-- Function Config -> RestingArmAngle -> ArmLength -> States
-- Takes the config and the base resting state and calculates a list of states based on the bump stop and limit strap.

-- Function Config -> ArmAngle -> ArmLength -> StepSize -> List of State
-- Where states has updated points for the axle side mounts

-- Function Config -> State -> Anti

-- Function with signature Config -> LowerArmAngle -> Result that can be saved in the CSV
-- this function takes teh angle of the arm and calculates the isnstant center based on it.

main :: IO ()
main = do
  result <- eitherDecodeFileStrict "config.json" :: IO (Either String Config)
  case result of
    Left err -> putStrLn err
    -- Right config -> print (calcState config Front (calcLowerArmAngle config Front) (calcLowerArmSideLength config Front))
    Right config -> print (calcUpperArmAxleMountLoc config Front (frontLowerArmAxleMountLoc config))
