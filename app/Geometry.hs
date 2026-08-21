{-# LANGUAGE DeriveGeneric #-}

module Geometry
  ( Point (..),
    UnitVector (..),
    radToDeg,
    calc2dDistance,
    calc3dDistance,
    calcXZAngle,
    calcDistanceBetweenCenters,
    calcDistanceToRadicalLine,
    calcPerpendicularOffset,
    calcCenterLinePoint,
    calcUnitVector,
    calcSteppedPerpendicular,
  )
where

import Data.Aeson (FromJSON)
import GHC.Generics (Generic)

data Point = Point
  { x :: Double,
    y :: Double,
    z :: Double
  }
  deriving (Show, Generic)

instance FromJSON Point

data UnitVector = UnitVector
  { uvx :: Double,
    uvy :: Double,
    uvz :: Double
  }
  deriving (Show, Generic)

radToDeg :: Double -> Double
radToDeg rads = rads * (180 / pi)

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

calcXZAngle :: Point -> Point -> Double
calcXZAngle p1 p2 = atan2 rise run
  where
    rise = z p2 - z p1
    run = x p2 - x p1

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

calcSteppedPerpendicular :: Point -> Double -> UnitVector -> (Point, Point)
calcSteppedPerpendicular p2 h u =
  (s1, s2)
  where
    s1 =
      Point
        { x = x p2 + h * (-uvz u),
          y = 0,
          z = z p2 + h * uvx u
        }
    s2 =
      Point
        { x = x p2 - h * (-uvz u),
          y = 0,
          z = z p2 - h * uvx u
        }
