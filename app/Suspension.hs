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
