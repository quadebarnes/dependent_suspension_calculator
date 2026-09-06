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
    calcTravel,
  )
where

import Config
import GHC.Generics (Generic)
import Geometry

data State = State
  { stateSystem :: System,
    stateLowerArmAngle :: Double,
    stateUpperArmAxleMountPos :: Point,
    stateLowerArmAxleMountPos :: Point
  }
  deriving (Show, Generic)

data System = Front | Rear
  deriving (Show)

data AxleConfig = AxleConfig
  { axlCfgSystem :: System,
    axlCfgAxleCenter :: Point,
    axlCfgUpperArmFrameMountLoc :: Point,
    axlCfgUpperArmAxleMountLoc :: Point,
    axlCfgLowerArmFrameMountLoc :: Point,
    axlCfgLowerArmAxleMountLoc :: Point
  }

data AxleAntis = AxleAntis
  { axlAntisLwrArmAngle :: Double,
    axlAntisBraking :: Double,
    axlAntisAcceleration :: Double
  }
  deriving (Show)

extractAxle :: Config -> System -> AxleConfig
extractAxle cfg sys =
  case sys of
    Front ->
      AxleConfig
        { axlCfgSystem = Front,
          axlCfgAxleCenter = Point 0 0 (configWheelRollingRadius cfg),
          axlCfgUpperArmFrameMountLoc = configFrontUpperArmFrameMountLoc cfg,
          axlCfgUpperArmAxleMountLoc = configFrontUpperArmAxleMountLoc cfg,
          axlCfgLowerArmFrameMountLoc = configFrontLowerArmFrameMountLoc cfg,
          axlCfgLowerArmAxleMountLoc = configFrontLowerArmAxleMountLoc cfg
        }
    Rear ->
      AxleConfig
        { axlCfgSystem = Rear,
          axlCfgAxleCenter = Point (configWheelbase cfg) 0 (configWheelRollingRadius cfg),
          axlCfgUpperArmFrameMountLoc = configRearUpperArmFrameMountLoc cfg,
          axlCfgUpperArmAxleMountLoc = configRearUpperArmAxleMountLoc cfg,
          axlCfgLowerArmFrameMountLoc = configRearLowerArmFrameMountLoc cfg,
          axlCfgLowerArmAxleMountLoc = configRearLowerArmAxleMountLoc cfg
        }

calcLowerArmProjectedLength :: AxleConfig -> Double
calcLowerArmProjectedLength axlConfig =
  calcProjectedDistance (axlCfgLowerArmFrameMountLoc axlConfig) (axlCfgLowerArmAxleMountLoc axlConfig)

calcUpperArmProjectedLength :: AxleConfig -> Double
calcUpperArmProjectedLength axlCfg =
  calcProjectedDistance (axlCfgUpperArmFrameMountLoc axlCfg) (axlCfgUpperArmAxleMountLoc axlCfg)

calcLowerArmAngle :: AxleConfig -> Double
calcLowerArmAngle axlCfg =
  calcProjectedAngle (axlCfgLowerArmAxleMountLoc axlCfg) (axlCfgLowerArmFrameMountLoc axlCfg)

calcAxleMountsDistance :: AxleConfig -> Double
calcAxleMountsDistance axlCfg =
  calcProjectedDistance (axlCfgUpperArmAxleMountLoc axlCfg) (axlCfgLowerArmAxleMountLoc axlCfg)

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
    p1 = axlCfgUpperArmFrameMountLoc axlCfg
    r0 = calcAxleMountsDistance axlCfg
    r1 = calcUpperArmProjectedLength axlCfg
    d = calcDistanceBetweenCenters p0 p1
    a = calcDistanceToRadicalLine r0 r1 d
    h = calcPerpendicularOffset r0 a
    u = calcUnitVector p0 p1
    p2 = calcCenterLinePoint p0 u a
    solutions = calcSteppedPerpendicular p2 h u
    solution = getCorrectMountSolution prev solutions

calcRestingState :: AxleConfig -> State
calcRestingState axlCfg =
  State
    { stateSystem = axlCfgSystem axlCfg,
      stateLowerArmAngle = calcLowerArmAngle axlCfg,
      stateUpperArmAxleMountPos = axlCfgUpperArmAxleMountLoc axlCfg,
      stateLowerArmAxleMountPos = axlCfgLowerArmAxleMountLoc axlCfg
    }

calcState :: AxleConfig -> Point -> Double -> State
calcState axlCfg prevUprArmAxleMountLoc lwrArmAngle =
  State
    { stateSystem = axlCfgSystem axlCfg,
      stateLowerArmAngle = lwrArmAngle,
      stateUpperArmAxleMountPos = upprLoc,
      stateLowerArmAxleMountPos = lwrLoc
    }
  where
    lwrLoc =
      Point
        { x = x (axlCfgLowerArmFrameMountLoc axlCfg) - lwrArmProjectedLength * cos lwrArmAngle,
          y = y (axlCfgLowerArmAxleMountLoc axlCfg),
          z = z (axlCfgLowerArmFrameMountLoc axlCfg) - lwrArmProjectedLength * sin lwrArmAngle
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
    zlf = z (axlCfgLowerArmFrameMountLoc axlCfg)
    zla = z (stateLowerArmAxleMountPos state)
    xlf = x (axlCfgLowerArmFrameMountLoc axlCfg)
    xla = x (stateLowerArmAxleMountPos state)
    mLower = (zlf - zla) / (xlf - xla)
    bLower = zla - mLower * xla

    zuf = z (axlCfgUpperArmFrameMountLoc axlCfg)
    zua = z (stateUpperArmAxleMountPos state)
    xuf = x (axlCfgUpperArmFrameMountLoc axlCfg)
    xua = x (stateUpperArmAxleMountPos state)
    mUpper = (zuf - zua) / (xuf - xua)
    bUpper = zua - mUpper * xua

    xic = (bUpper - bLower) / (mLower - mUpper)
    zic = mLower * xic + bLower

calcHousingOrientation :: State -> Double
calcHousingOrientation state = atan2 (zua - zla) (xua - xla)
  where
    xla = x (stateLowerArmAxleMountPos state)
    xua = x (stateUpperArmAxleMountPos state)
    zla = z (stateLowerArmAxleMountPos state)
    zua = z (stateUpperArmAxleMountPos state)

calcHousingOrientationChange :: AxleConfig -> State -> Double
calcHousingOrientationChange axlCfg state =
  calcAngleDifference r0 r1
  where
    restingState = calcRestingState axlCfg
    r0 = calcHousingOrientation restingState
    r1 = calcHousingOrientation state

calcAxleCenter :: Config -> AxleConfig -> State -> Point
calcAxleCenter cfg axlCfg state =
  setPointY (applyOffset2d pla rotatedOffset) 0
  where
    pla = stateLowerArmAxleMountPos state
    hr = calcHousingOrientationChange axlCfg state
    p0 =
      case axlCfgSystem axlCfg of
        Front -> configFrontLowerArmAxleMountLoc cfg
        Rear -> configRearLowerArmAxleMountLoc cfg
    p1 = axlCfgAxleCenter axlCfg
    offset = calcPointOffset p0 p1
    rotatedOffset = calcRotatedOffset offset hr

calcAnti :: Config -> AxleConfig -> State -> AxleAntis
calcAnti cfg axlCfg state =
  AxleAntis
    { axlAntisLwrArmAngle = stateLowerArmAngle state,
      axlAntisBraking = ((l * brakeB * slope) / hcg) * 100,
      axlAntisAcceleration = ((l * accelB * slope) / hcg) * 100
    }
  where
    l = configWheelbase cfg
    accelB = case axlCfgSystem axlCfg of
      Front -> configDriveBias cfg
      Rear -> 1 - configDriveBias cfg
    brakeB = case axlCfgSystem axlCfg of
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
    startingUpperArmMountPos = axlCfgUpperArmAxleMountLoc axlCfg
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

calcTravel :: AxleConfig -> Double -> Double
calcTravel axlCfg lwrArmAngle =
  a * tan theta
  where
    restingState = calcRestingState axlCfg
    a = calcLowerArmProjectedLength axlCfg
    theta = lwrArmAngle - stateLowerArmAngle restingState
