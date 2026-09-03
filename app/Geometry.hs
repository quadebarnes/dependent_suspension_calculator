{-# LANGUAGE DeriveGeneric #-}

module Geometry
  ( Point (..),
    UnitVector (..),
    calcProjectedDistance,
    calc3dDistance,
    calcProjectedAngle,
    calcDistanceBetweenCenters,
    calcDistanceToRadicalLine,
    calcPerpendicularOffset,
    calcCenterLinePoint,
    calcUnitVector,
    calcSteppedPerpendicular,
    setPointY,
    calcAngleDifference,
    applyOffset2d,
    calcPointOffset,
    calcRotatedOffset,
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

data Offset2d = Offset2d
  { offx :: Double,
    offz :: Double
  }
  deriving (Show)

calcProjectedDistance :: Point -> Point -> Double
calcProjectedDistance p1 p2 = sqrt (dx * dx + dz * dz)
  where
    dx = x p2 - x p1
    dz = z p2 - z p1

calc3dDistance :: Point -> Point -> Double
calc3dDistance p1 p2 = sqrt (dx * dx + dy * dy + dz * dz)
  where
    dx = x p2 - x p1
    dy = y p2 - y p1
    dz = z p2 - z p1

calcProjectedAngle :: Point -> Point -> Double
calcProjectedAngle p1 p2 = atan2 rise run
  where
    rise = z p2 - z p1
    run = x p2 - x p1

calcDistanceBetweenCenters :: Point -> Point -> Double
calcDistanceBetweenCenters p0 p1 = abs (calcProjectedDistance p1 p0)

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

setPointY :: Point -> Double -> Point
setPointY p0 newY =
  Point
    { x = x p0,
      y = newY,
      z = z p0
    }

calcAngleDifference :: Double -> Double -> Double
calcAngleDifference r0 r1 = r1 - r0

calcPointOffset :: Point -> Point -> Offset2d
calcPointOffset p0 p1 =
  Offset2d
    { offx = x p1 - x p0,
      offz = z p1 - z p0
    }

calcRotatedOffset :: Offset2d -> Double -> Offset2d
calcRotatedOffset o r0 =
  Offset2d
    { offx = offx o * cos r0 - offz o * sin r0,
      offz = offx o * sin r0 + offz o * cos r0
    }

applyOffset2d :: Point -> Offset2d -> Point
applyOffset2d p0 o =
  Point
    { x = x p0 + offx o,
      y = y p0,
      z = z p0 + offz o
    }
