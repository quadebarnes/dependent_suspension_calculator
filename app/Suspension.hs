{-# LANGUAGE DeriveGeneric #-}

module Suspension
  ( State (..),
    System (..),
    AxleConfig (..),
    AxleAntis (..),
    calcRestingLwrArmAngle,
    calcState,
    extractAxle,
    calcAnti,
    sweepStates,
    sweepAnti,
    getTrvl,
    sweepTrvl,
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

calcRestingLwrArmProjLen :: AxleConfig -> Double
calcRestingLwrArmProjLen axlConfig =
  calcProjectedDistance (axlCfgLowerArmFrameMountLoc axlConfig) (axlCfgLowerArmAxleMountLoc axlConfig)

calcRestingUpprArmProjLen :: AxleConfig -> Double
calcRestingUpprArmProjLen axlCfg =
  calcProjectedDistance (axlCfgUpperArmFrameMountLoc axlCfg) (axlCfgUpperArmAxleMountLoc axlCfg)

calcRestingLwrArmAngle :: AxleConfig -> Double
calcRestingLwrArmAngle axlCfg =
  calcProjectedAngle (axlCfgLowerArmAxleMountLoc axlCfg) (axlCfgLowerArmFrameMountLoc axlCfg)

calcAxlMntsDist :: AxleConfig -> Double
calcAxlMntsDist axlCfg =
  calcProjectedDistance (axlCfgUpperArmAxleMountLoc axlCfg) (axlCfgLowerArmAxleMountLoc axlCfg)

choseMntSolution :: Point -> (Point, Point) -> Point
choseMntSolution prev (s1, s2) =
  if s1d < s2d then s1 else s2
  where
    s1d = calc3dDistance prev s1
    s2d = calc3dDistance prev s2

-- Note: p0 is the lowerArmAxleMountLoc
getUpprArmAxlMntLoc :: AxleConfig -> Point -> Point -> Point
getUpprArmAxlMntLoc axlCfg prev p0 = setPointY solution (y prev)
  where
    p1 = axlCfgUpperArmFrameMountLoc axlCfg
    r0 = calcAxlMntsDist axlCfg
    r1 = calcRestingUpprArmProjLen axlCfg
    d = calcDistanceBetweenCenters p0 p1
    a = calcDistanceToRadicalLine r0 r1 d
    h = calcPerpendicularOffset r0 a
    u = calcUnitVector p0 p1
    p2 = calcCenterLinePoint p0 u a
    solutions = calcSteppedPerpendicular p2 h u
    solution = choseMntSolution prev solutions

calcRestingState :: AxleConfig -> State
calcRestingState axlCfg =
  State
    { stateSystem = axlCfgSystem axlCfg,
      stateLowerArmAngle = calcRestingLwrArmAngle axlCfg,
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
    lwrArmProjectedLength = calcRestingLwrArmProjLen axlCfg
    upprLoc = getUpprArmAxlMntLoc axlCfg prevUprArmAxleMountLoc lwrLoc

getIC :: AxleConfig -> State -> Point
getIC axlCfg state =
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

getHousignOrient :: State -> Double
getHousignOrient state = atan2 (zua - zla) (xua - xla)
  where
    xla = x (stateLowerArmAxleMountPos state)
    xua = x (stateUpperArmAxleMountPos state)
    zla = z (stateLowerArmAxleMountPos state)
    zua = z (stateUpperArmAxleMountPos state)

getHousignOrientChng :: AxleConfig -> State -> Double
getHousignOrientChng axlCfg state =
  calcAngleDifference r0 r1
  where
    restingState = calcRestingState axlCfg
    r0 = getHousignOrient restingState
    r1 = getHousignOrient state

getAxlCntr :: Config -> AxleConfig -> State -> Point
getAxlCntr cfg axlCfg state =
  setPointY (applyOffset2d pla rotatedOffset) 0
  where
    pla = stateLowerArmAxleMountPos state
    hr = getHousignOrientChng axlCfg state
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
    ic = getIC axlCfg state
    axleCenter = getAxlCntr cfg axlCfg state
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

sweepAnti :: Config -> AxleConfig -> [State] -> [AxleAntis]
sweepAnti cfg axlCfg =
  map calcSweepAnti
  where
    calcSweepAnti = calcAnti cfg axlCfg

getTrvl :: Config -> AxleConfig -> State -> Double
getTrvl cfg axlCfg state =
  z (getAxlCntr cfg axlCfg state) - configWheelRollingRadius cfg

sweepTrvl :: Config -> AxleConfig -> [State] -> [Double]
sweepTrvl cfg axlCfg =
  map calcSweepTrvl
  where
    calcSweepTrvl = getTrvl cfg axlCfg
