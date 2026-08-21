{-# LANGUAGE DeriveGeneric #-}

module Suspension
  ( State (..),
    System (..),
    calcLowerArmLength,
    calcLowerArmProjectedLength,
    calcUpperArmProjectedLength,
    calcLowerArmAngle,
    calcAxleMountsDistance,
    calcUpperArmAxleMountLoc,
    calcState,
    calcInstantCenter,
  )
where

import Config
import GHC.Generics (Generic)
import Geometry

data State = State
  { lowerArmAngle :: Double,
    upperArmAxleMountPos :: Point,
    lowerArmAxleMountPos :: Point
  }
  deriving (Show, Generic)

data System = Front | Rear

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

calcLowerArmAngle :: Config -> System -> Double
calcLowerArmAngle cfg sys = case sys of
  Front -> calcXZAngle (frontLowerArmAxleMountLoc cfg) (frontLowerArmFrameMountLoc cfg)
  Rear -> calcXZAngle (rearLowerArmAxleMountLoc cfg) (rearLowerArmFrameMountLoc cfg)

calcAxleMountsDistance :: Config -> System -> Double
calcAxleMountsDistance cfg sys = case sys of
  Front -> calc2dDistance (frontUpperArmAxleMountLoc cfg) (frontLowerArmAxleMountLoc cfg)
  Rear -> calc2dDistance (rearUpperArmAxleMountLoc cfg) (rearLowerArmAxleMountLoc cfg)

getCorrectMountSolution :: Point -> (Point, Point) -> Point
getCorrectMountSolution prev (s1, s2) =
  if s1d < s2d then s1 else s2
  where
    s1d = calc3dDistance prev s1
    s2d = calc3dDistance prev s2

-- Note: p0 is the lowerArmAxleMountLoc
calcUpperArmAxleMountLoc :: Config -> System -> Point -> Point -> Point
calcUpperArmAxleMountLoc cfg sys prev p0 = getCorrectMountSolution prev solutions
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
    solutions = calcSteppedPerpendicular p2 h u

calcState :: Config -> System -> Point -> Double -> State
calcState cfg sys prevUprArmAxleMountLoc lwrArmAngle = case sys of
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
      upprLoc = calcUpperArmAxleMountLoc cfg sys prevUprArmAxleMountLoc lwrLoc
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
      upprLoc = calcUpperArmAxleMountLoc cfg sys prevUprArmAxleMountLoc lwrLoc
  where
    lwrArmProjectedLength = calcLowerArmProjectedLength cfg sys

calcInstantCenter :: Config -> System -> State -> Point
calcInstantCenter cfg sys state = case sys of
  Front ->
    Point
      { x = xic,
        y = 0,
        z = zic
      }
    where
      zlf = z (frontLowerArmFrameMountLoc cfg)
      zla = z (lowerArmAxleMountPos state)
      xlf = x (frontLowerArmFrameMountLoc cfg)
      xla = x (lowerArmAxleMountPos state)
      mLower = (zlf - zla) / (xlf - xla)
      bLower = zla - mLower * xla

      zuf = z (frontUpperArmFrameMountLoc cfg)
      zua = z (upperArmAxleMountPos state)
      xuf = x (frontUpperArmFrameMountLoc cfg)
      xua = x (upperArmAxleMountPos state)
      mUpper = (zuf - zua) / (xuf - xua)
      bUpper = zua - mUpper * xua

      xic = (bUpper - bLower) / (mLower - mUpper)
      zic = mLower * xic + bLower
  Rear ->
    Point
      { x = xic,
        y = 0,
        z = zic
      }
  where
    zlf = z (rearLowerArmFrameMountLoc cfg)
    zla = z (lowerArmAxleMountPos state)
    xlf = x (rearLowerArmFrameMountLoc cfg)
    xla = x (lowerArmAxleMountPos state)
    mLower = (zlf - zla) / (xlf - xla)
    bLower = zla - mLower * xla

    zuf = z (rearUpperArmFrameMountLoc cfg)
    zua = z (upperArmAxleMountPos state)
    xuf = x (rearUpperArmFrameMountLoc cfg)
    xua = x (upperArmAxleMountPos state)
    mUpper = (zuf - zua) / (xuf - xua)
    bUpper = zua - mUpper * xua

    xic = (bUpper - bLower) / (mLower - mUpper)
    zic = mLower * xic + bLower
