{-# LANGUAGE DeriveGeneric #-}

module Suspension
  ( State (..),
    System (..),
    AxleConfig (..),
    -- calcLowerArmLength,
    calcLowerArmProjectedLength,
    calcUpperArmProjectedLength,
    calcLowerArmAngle,
    calcAxleMountsDistance,
    calcUpperArmAxleMountLoc,
    calcState,
    calcInstantCenter,
    extractAxle,
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

data AxleConfig = AxleConfig
  { upperArmFrameMountLoc :: Point,
    upperArmAxleMountLoc :: Point,
    lowerArmFrameMountLoc :: Point,
    lowerArmAxleMountLoc :: Point
  }

extractAxle :: Config -> System -> AxleConfig
extractAxle cfg sys =
  case sys of
    Front ->
      AxleConfig
        { upperArmFrameMountLoc = frontUpperArmFrameMountLoc cfg,
          upperArmAxleMountLoc = frontUpperArmAxleMountLoc cfg,
          lowerArmFrameMountLoc = frontLowerArmFrameMountLoc cfg,
          lowerArmAxleMountLoc = frontLowerArmAxleMountLoc cfg
        }
    Rear ->
      AxleConfig
        { upperArmFrameMountLoc = rearUpperArmFrameMountLoc cfg,
          upperArmAxleMountLoc = rearUpperArmAxleMountLoc cfg,
          lowerArmFrameMountLoc = rearLowerArmFrameMountLoc cfg,
          lowerArmAxleMountLoc = rearLowerArmAxleMountLoc cfg
        }

-- calcLowerArmLength :: Config -> System -> Double
-- calcLowerArmLength cfg sys = case sys of
--   Front -> calc3dDistance (frontLowerArmFrameMountLoc cfg) (frontLowerArmAxleMountLoc cfg)
--   Rear -> calc3dDistance (rearLowerArmAxleMountLoc cfg) (rearLowerArmFrameMountLoc cfg)

calcLowerArmProjectedLength :: AxleConfig -> Double
calcLowerArmProjectedLength axlConfig =
  calcProjectedDistance (lowerArmFrameMountLoc axlConfig) (lowerArmAxleMountLoc axlConfig)

calcUpperArmProjectedLength :: AxleConfig -> Double
calcUpperArmProjectedLength axlCfg =
  calcProjectedDistance (upperArmFrameMountLoc axlCfg) (upperArmAxleMountLoc axlCfg)

calcLowerArmAngle :: AxleConfig -> Double
calcLowerArmAngle axlCfg =
  calcProjectedAngle (lowerArmAxleMountLoc axlCfg) (lowerArmFrameMountLoc axlCfg)

calcAxleMountsDistance :: AxleConfig -> Double
calcAxleMountsDistance axlCfg =
  calcProjectedDistance (upperArmAxleMountLoc axlCfg) (lowerArmAxleMountLoc axlCfg)

getCorrectMountSolution :: Point -> (Point, Point) -> Point
getCorrectMountSolution prev (s1, s2) =
  if s1d < s2d then s1 else s2
  where
    s1d = calc3dDistance prev s1
    s2d = calc3dDistance prev s2

-- Note: p0 is the lowerArmAxleMountLoc
calcUpperArmAxleMountLoc :: AxleConfig -> Point -> Point -> Point
calcUpperArmAxleMountLoc axlCfg prev p0 = setPointY solution (y prev)
  where
    p1 = upperArmFrameMountLoc axlCfg
    r0 = calcAxleMountsDistance axlCfg
    r1 = calcUpperArmProjectedLength axlCfg
    d = calcDistanceBetweenCenters p0 p1
    a = calcDistanceToRadicalLine r0 r1 d
    h = calcPerpendicularOffset r0 a
    u = calcUnitVector p0 p1
    p2 = calcCenterLinePoint p0 u a
    solutions = calcSteppedPerpendicular p2 h u
    solution = getCorrectMountSolution prev solutions

calcState :: AxleConfig -> Point -> Double -> State
calcState axlCfg prevUprArmAxleMountLoc lwrArmAngle =
  State
    { lowerArmAngle = lwrArmAngle,
      upperArmAxleMountPos = upprLoc,
      lowerArmAxleMountPos = lwrLoc
    }
  where
    lwrLoc =
      Point
        { x = x (lowerArmFrameMountLoc axlCfg) - lwrArmProjectedLength * cos lwrArmAngle,
          y = y (lowerArmAxleMountLoc axlCfg),
          z = z (lowerArmFrameMountLoc axlCfg) - lwrArmProjectedLength * sin lwrArmAngle
        }
    lwrArmProjectedLength = calcLowerArmProjectedLength axlCfg
    upprLoc = calcUpperArmAxleMountLoc axlCfg prevUprArmAxleMountLoc lwrLoc

calcInstantCenter :: AxleConfig -> State -> Point
calcInstantCenter axlCfg state =
  Point
    { x = xic,
      y = 0,
      z = zic
    }
  where
    zlf = z (lowerArmFrameMountLoc axlCfg)
    zla = z (lowerArmAxleMountPos state)
    xlf = x (lowerArmFrameMountLoc axlCfg)
    xla = x (lowerArmAxleMountPos state)
    mLower = (zlf - zla) / (xlf - xla)
    bLower = zla - mLower * xla

    zuf = z (upperArmFrameMountLoc axlCfg)
    zua = z (upperArmAxleMountPos state)
    xuf = x (upperArmFrameMountLoc axlCfg)
    xua = x (upperArmAxleMountPos state)
    mUpper = (zuf - zua) / (xuf - xua)
    bUpper = zua - mUpper * xua

    xic = (bUpper - bLower) / (mLower - mUpper)
    zic = mLower * xic + bLower
