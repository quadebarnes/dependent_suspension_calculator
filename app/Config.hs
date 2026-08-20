{-# LANGUAGE DeriveGeneric #-}

module Config
  ( Config (..),
  )
where

import Data.Aeson (FromJSON)
import GHC.Generics (Generic)
import Geometry (Point)

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
