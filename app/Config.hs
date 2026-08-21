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
    { wheelbase = wheelbase rawCfg,
      brakeBias = brakeBias rawCfg,
      driveBias = driveBias rawCfg,
      mass = mass rawCfg,
      massSprung = massSprung rawCfg,
      massUnsprung = massUnsprung rawCfg,
      wheelRollingRadius = wheelRollingRadius rawCfg,
      frontShockAxleMountingLoc = frontShockAxleMountingLoc rawCfg,
      frontShockFrameMountingLoc = frontShockFrameMountingLoc rawCfg,
      frontShockExtendedLength = frontShockExtendedLength rawCfg,
      frontShockCompressedLength = frontShockCompressedLength rawCfg,
      rearShockAxleMountingLoc = normalizePoint (rearShockAxleMountingLoc rawCfg) (wheelbase rawCfg),
      rearShockFrameMountingLoc = normalizePoint (rearShockFrameMountingLoc rawCfg) (wheelbase rawCfg),
      rearShockExtendedLength = rearShockExtendedLength rawCfg,
      rearShockCompressedLength = rearShockCompressedLength rawCfg,
      frontUpperArmFrameMountLoc = frontUpperArmFrameMountLoc rawCfg,
      frontUpperArmAxleMountLoc = frontUpperArmAxleMountLoc rawCfg,
      frontLowerArmFrameMountLoc = frontLowerArmFrameMountLoc rawCfg,
      frontLowerArmAxleMountLoc = frontLowerArmAxleMountLoc rawCfg,
      frontTrackBarAxleMountLoc = frontTrackBarAxleMountLoc rawCfg,
      frontTrackBarFrameMountLoc = frontTrackBarFrameMountLoc rawCfg,
      rearUpperArmFrameMountLoc = normalizePoint (rearUpperArmFrameMountLoc rawCfg) (wheelbase rawCfg),
      rearUpperArmAxleMountLoc = normalizePoint (rearUpperArmAxleMountLoc rawCfg) (wheelbase rawCfg),
      rearLowerArmFrameMountLoc = normalizePoint (rearLowerArmFrameMountLoc rawCfg) (wheelbase rawCfg),
      rearLowerArmAxleMountLoc = normalizePoint (rearLowerArmAxleMountLoc rawCfg) (wheelbase rawCfg)
    }
