{-# LANGUAGE DeriveGeneric #-}

module Main (main) where

import Data.Aeson (FromJSON, eitherDecodeFileStrict)
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
calc2dDistance p1 p2 = sqrt (dx * dx + dz * dz)
  where
    dx = x p2 - x p1
    dz = z p2 - z p1

calc3dDistance :: Point -> Point -> Double
calc3dDistance p1 p2 = sqrt (dx * dx + dy * dy + dz * dz)
  where
    dx = x p2 - x p1
    dy = y p2 - y p1
    dz = z p2 - z p1

calcLowerArmLength :: Config -> System -> Double
calcLowerArmLength cfg sys = case sys of
  Front -> calc3dDistance (frontLowerArmFrameMountLoc cfg) (frontLowerArmAxleMountLoc cfg)
  Rear -> calc3dDistance (rearLowerArmAxleMountLoc cfg) (rearLowerArmFrameMountLoc cfg)

calcLowerArmProjectedLength :: Config -> System -> Double
calcLowerArmProjectedLength cfg sys = case sys of
  Front -> calc2dDistance (frontLowerArmFrameMountLoc cfg) (frontLowerArmAxleMountLoc cfg)
  Rear -> calc2dDistance (rearLowerArmAxleMountLoc cfg) (rearLowerArmFrameMountLoc cfg)

calcUpperArmProjectedLength :: Config -> System -> Double
calcUpperArmProjectedLength cfg sys = case sys of
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
calcDistanceToRadicalLine r0 r1 d = (r0 * r0 - r1 * r1 + d * d) / (2 * d)

calcPerpendicularOffset :: Double -> Double -> Double
calcPerpendicularOffset r0 a = sqrt (r0 * r0 - a * a)

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
    d = sqrt (vx * vx + vz * vz)

calcSteppedPerpendicular :: Point -> Double -> UnitVector -> Point
calcSteppedPerpendicular p2 h u =
  Point
    { x = x p2 + h * (-uvz u),
      y = 0,
      z = z p2 + h * uvx u
    }

-- Note: p0 is the lowerArmAxleMountLoc
calcUpperArmAxleMountLoc :: Config -> System -> Point -> Point
calcUpperArmAxleMountLoc cfg sys p0 = calcSteppedPerpendicular p2 h u
  where
    p1 = case sys of
      Front -> frontUpperArmFrameMountLoc cfg
      Rear -> rearUpperArmFrameMountLoc cfg
    r0 = calcAxleMountsDistance cfg sys
    r1 = calcUpperArmProjectedLength cfg sys
    d = calcDistanceBetweenCenters p0 p1
    a = calcDistanceToRadicalLine r0 r1 d
    h = calcPerpendicularOffset r0 a
    u = calcUnitVector p0 p1
    p2 = calcCenterLinePoint p0 u a

calcState :: Config -> System -> Double -> State
calcState cfg sys lwrArmAngle = case sys of
  Front ->
    State
      { lowerArmAngle = lwrArmAngle,
        upperArmAxleMountPos = upprLoc,
        lowerArmAxleMountPos = lwrLoc
      }
    where
      lwrLoc =
        Point
          { x = x (frontLowerArmFrameMountLoc cfg) - lwrArmProjectedLength * cos lwrArmAngle,
            y = y (frontLowerArmAxleMountLoc cfg),
            z = z (frontLowerArmFrameMountLoc cfg) - lwrArmProjectedLength * sin lwrArmAngle
          }
      upprLoc = calcUpperArmAxleMountLoc cfg sys lwrLoc
  Rear ->
    State
      { lowerArmAngle = lwrArmAngle,
        upperArmAxleMountPos = upprLoc,
        lowerArmAxleMountPos = lwrLoc
      }
    where
      lwrLoc =
        Point
          { x = x (rearLowerArmFrameMountLoc cfg) - lwrArmProjectedLength * cos lwrArmAngle,
            y = y (rearLowerArmAxleMountLoc cfg),
            z = z (rearLowerArmFrameMountLoc cfg) - lwrArmProjectedLength * sin lwrArmAngle
          }
      upprLoc = calcUpperArmAxleMountLoc cfg sys lwrLoc
  where
    lwrArmProjectedLength = calcLowerArmProjectedLength cfg sys

main :: IO ()
main = do
  result <- eitherDecodeFileStrict "config.json" :: IO (Either String Config)
  case result of
    Left err -> putStrLn err
    Right config -> print (calcState config Front (calcLowerArmAngle config Front))
