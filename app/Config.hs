{-# LANGUAGE DeriveGeneric #-}

module Config
  ( Config (..),
    normalize,
  )
where

import Data.Aeson (FromJSON)
import GHC.Generics (Generic)
import Geometry (Point (..))

data Config = Config
  { configWheelbase :: Double,
    configBrakeBias :: Double,
    configDriveBias :: Double,
    configMass :: Double,
    configMassSprung :: Double,
    configMassUnsprung :: Double,
    configWheelRollingRadius :: Double,
    configSprungCGHeight :: Double,
    configFrontShockAxleMountingLoc :: Point,
    configFrontShockFrameMountingLoc :: Point,
    configFrontShockExtendedLength :: Double,
    configFrontShockCompressedLength :: Double,
    configRearShockAxleMountingLoc :: Point,
    configRearShockFrameMountingLoc :: Point,
    configRearShockExtendedLength :: Double,
    configRearShockCompressedLength :: Double,
    configFrontUpperArmFrameMountLoc :: Point,
    configFrontUpperArmAxleMountLoc :: Point,
    configFrontLowerArmFrameMountLoc :: Point,
    configFrontLowerArmAxleMountLoc :: Point,
    configFrontTrackBarAxleMountLoc :: Point,
    configFrontTrackBarFrameMountLoc :: Point,
    configRearUpperArmFrameMountLoc :: Point,
    configRearUpperArmAxleMountLoc :: Point,
    configRearLowerArmFrameMountLoc :: Point,
    configRearLowerArmAxleMountLoc :: Point
  }
  deriving (Show, Generic)

instance FromJSON Config

normalizePoint :: Point -> Double -> Point
normalizePoint rawPoint wb =
  Point
    { x = wb - x rawPoint,
      y = y rawPoint,
      z = z rawPoint
    }

normalize :: Config -> Config
normalize rawCfg =
  Config
    { configWheelbase = configWheelbase rawCfg,
      configBrakeBias = configBrakeBias rawCfg,
      configDriveBias = configDriveBias rawCfg,
      configMass = configMass rawCfg,
      configMassSprung = configMassSprung rawCfg,
      configMassUnsprung = configMassUnsprung rawCfg,
      configWheelRollingRadius = configWheelRollingRadius rawCfg,
      configSprungCGHeight = configSprungCGHeight rawCfg,
      configFrontShockAxleMountingLoc = configFrontShockAxleMountingLoc rawCfg,
      configFrontShockFrameMountingLoc = configFrontShockFrameMountingLoc rawCfg,
      configFrontShockExtendedLength = configFrontShockExtendedLength rawCfg,
      configFrontShockCompressedLength = configFrontShockCompressedLength rawCfg,
      configRearShockAxleMountingLoc = normalizePoint (configRearShockAxleMountingLoc rawCfg) (configWheelbase rawCfg),
      configRearShockFrameMountingLoc = normalizePoint (configRearShockFrameMountingLoc rawCfg) (configWheelbase rawCfg),
      configRearShockExtendedLength = configRearShockExtendedLength rawCfg,
      configRearShockCompressedLength = configRearShockCompressedLength rawCfg,
      configFrontUpperArmFrameMountLoc = configFrontUpperArmFrameMountLoc rawCfg,
      configFrontUpperArmAxleMountLoc = configFrontUpperArmAxleMountLoc rawCfg,
      configFrontLowerArmFrameMountLoc = configFrontLowerArmFrameMountLoc rawCfg,
      configFrontLowerArmAxleMountLoc = configFrontLowerArmAxleMountLoc rawCfg,
      configFrontTrackBarAxleMountLoc = configFrontTrackBarAxleMountLoc rawCfg,
      configFrontTrackBarFrameMountLoc = configFrontTrackBarFrameMountLoc rawCfg,
      configRearUpperArmFrameMountLoc = normalizePoint (configRearUpperArmFrameMountLoc rawCfg) (configWheelbase rawCfg),
      configRearUpperArmAxleMountLoc = normalizePoint (configRearUpperArmAxleMountLoc rawCfg) (configWheelbase rawCfg),
      configRearLowerArmFrameMountLoc = normalizePoint (configRearLowerArmFrameMountLoc rawCfg) (configWheelbase rawCfg),
      configRearLowerArmAxleMountLoc = normalizePoint (configRearLowerArmAxleMountLoc rawCfg) (configWheelbase rawCfg)
    }
