{-# LANGUAGE DeriveGeneric #-}

module Suspension
  ( State (..),
    System (..),
    AxleConfig (..),
    AxleAntis (..),
    calcLowerArmProjectedLength,
    calcUpperArmProjectedLength,
    calcLowerArmAngle,
    calcAxleMountsDistance,
    calcUpperArmAxleMountLoc,
    calcState,
    calcInstantCenter,
    extractAxle,
    calcHousingOrientation,
    calcAnti,
    sweepStates,
    sweepAnti,
    calcAxleCenter,
  )
where

import Config
import GHC.Generics (Generic)
import Geometry

data State = State
  { sys :: System,
    lowerArmAngle :: Double,
    upperArmAxleMountPos :: Point,
    lowerArmAxleMountPos :: Point
  }
  deriving (Show, Generic)

data System = Front | Rear
  deriving (Show)

data AxleConfig = AxleConfig
  { system :: System,
    axleCenter :: Point,
    upperArmFrameMountLoc :: Point,
    upperArmAxleMountLoc :: Point,
    lowerArmFrameMountLoc :: Point,
    lowerArmAxleMountLoc :: Point
  }

data AxleAntis = AxleAntis
  { lwrArmAngle :: Double,
    braking :: Double,
    acceleration :: Double
  }
  deriving (Show)

extractAxle :: Config -> System -> AxleConfig
extractAxle cfg sys =
  case sys of
    Front ->
      AxleConfig
        { system = Front,
          axleCenter = Point 0 0 (configWheelRollingRadius cfg),
          upperArmFrameMountLoc = configFrontUpperArmFrameMountLoc cfg,
          upperArmAxleMountLoc = configFrontUpperArmAxleMountLoc cfg,
          lowerArmFrameMountLoc = configFrontLowerArmFrameMountLoc cfg,
          lowerArmAxleMountLoc = configFrontLowerArmAxleMountLoc cfg
        }
    Rear ->
      AxleConfig
        { system = Rear,
          axleCenter = Point (configWheelbase cfg) 0 (configWheelRollingRadius cfg),
          upperArmFrameMountLoc = configRearUpperArmFrameMountLoc cfg,
          upperArmAxleMountLoc = configRearUpperArmAxleMountLoc cfg,
          lowerArmFrameMountLoc = configRearLowerArmFrameMountLoc cfg,
          lowerArmAxleMountLoc = configRearLowerArmAxleMountLoc cfg
        }

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

calcRestingState :: Config -> AxleConfig -> State
calcRestingState cfg axlCfg =
  State
    { sys = system axlCfg,
      lowerArmAngle = calcLowerArmAngle axlCfg,
      upperArmAxleMountPos = upperArmAxleMountLoc axlCfg,
      lowerArmAxleMountPos = lowerArmAxleMountLoc axlCfg
    }

calcState :: AxleConfig -> Point -> Double -> State
calcState axlCfg prevUprArmAxleMountLoc lwrArmAngle =
  State
    { sys = system axlCfg,
      lowerArmAngle = lwrArmAngle,
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

calcHousingOrientation :: State -> Double
calcHousingOrientation state = atan2 (zua - zla) (xua - xla)
  where
    xla = x (lowerArmAxleMountPos state)
    xua = x (upperArmAxleMountPos state)
    zla = z (lowerArmAxleMountPos state)
    zua = z (upperArmAxleMountPos state)

calcHousingOrientationChange :: Config -> AxleConfig -> State -> Double
calcHousingOrientationChange cfg axlCfg state =
  calcAngleDifference r0 r1
  where
    restingState = calcRestingState cfg axlCfg
    r0 = calcHousingOrientation restingState
    r1 = calcHousingOrientation state

calcAxleCenter :: Config -> AxleConfig -> State -> Point
calcAxleCenter cfg axlCfg state =
  setPointY (applyOffset2d pla rotatedOffset) 0
  where
    pla = lowerArmAxleMountPos state
    hr = calcHousingOrientationChange cfg axlCfg state
    p0 =
      case system axlCfg of
        Front -> configFrontLowerArmAxleMountLoc cfg
        Rear -> configRearLowerArmAxleMountLoc cfg
    p1 = axleCenter axlCfg
    offset = calcPointOffset p0 p1
    rotatedOffset = calcRotatedOffset offset hr

calcAnti :: Config -> AxleConfig -> State -> AxleAntis
calcAnti cfg axlCfg state =
  AxleAntis
    { lwrArmAngle = lowerArmAngle state,
      braking = ((l * brakeB * slope) / hcg) * 100,
      acceleration = ((l * accelB * slope) / hcg) * 100
    }
  where
    l = configWheelbase cfg
    accelB = case system axlCfg of
      Front -> configDriveBias cfg
      Rear -> 1 - configDriveBias cfg
    brakeB = case system axlCfg of
      Front -> configBrakeBias cfg
      Rear -> 1 - configBrakeBias cfg
    ic = calcInstantCenter axlCfg state
    axleCenter = calcAxleCenter cfg axlCfg state
    xic = abs (x ic - x axleCenter)
    slope = z ic / xic
    hcg = configSprungCGHeight cfg

sweepStates :: AxleConfig -> Double -> Double -> Int -> [State]
sweepStates axlCfg startAngle angleChange numSteps =
  reverse (map calcSweepState droopSweepAngles) ++ map calcSweepState compressingSweepAngles
  where
    calcSweepState = calcState axlCfg startingUpperArmMountPos
    startingUpperArmMountPos = upperArmAxleMountLoc axlCfg
    compressEndAngle = startAngle + angleChange
    droopSweepAngle = startAngle - angleChange
    stepSize = angleChange / fromIntegral numSteps
    compressingSweepAngles = [startAngle, startAngle + stepSize .. compressEndAngle]
    droopSweepAngles = [startAngle - stepSize, (startAngle - stepSize) - stepSize .. droopSweepAngle]

sweepAnti :: Config -> AxleConfig -> Double -> Double -> Int -> [AxleAntis]
sweepAnti cfg axlCfg startAngle angleChange numSteps =
  map calcSweepAnti states
  where
    calcSweepAnti = calcAnti cfg axlCfg
    states = sweepStates axlCfg startAngle angleChange numSteps
