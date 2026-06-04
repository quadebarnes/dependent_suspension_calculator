module Main where

-- As a percentage, the drive force of the front.
frontDriveForce :: Float
frontDriveForce = 0.50

-- As a percentage, the braking force of the front.
frontBrakingForce :: Float
frontBrakingForce = 0.60

-- Used to find the horizontal weigth distribution of the vehicle.
horDistribution :: Float -> Float -> Float
horDistribution frontWheelWeight totalWeight = frontWheelWeight / totalWeight

-- Used to find the horizontal CG. The result is the distance from the rear tires.
horCG :: Float -> Float -> Float
horCG distribution wheelBase = distribution * wheelBase

-- An intermediate step to calcualte the Horizontal CG. After lifting the vehicle up enter the horizontal distance between the wheels instead of the wheelbase.
raisedHorCg :: Float -> Float -> Float
raisedHorCg = horCG

-- Used to find the CG of the sprung mass.
sprungMassCGHeight :: Float -> Float -> Float -> Float -> Float -> Float
sprungMassCGHeight massTotal cgHeightVehicle massUnsprung wheelCenterFromGround massSprung = (massTotal * cgHeightVehicle - massUnsprung * wheelCenterFromGround) / massSprung

anti :: Float -> Float -> Float -> Float -> Float -> Float
anti wheelbase bias zInstantCenter xInstantCenter sprungCGHeight = (wheelbase * bias * (zInstantCenter / xInstantCenter)) / sprungCGHeight

main :: IO ()
main = do
  print (horDistribution 500 1000)
  print (horCG 0.5 170.0)
  print ()

-- Get some fancy kind of library that handels arguments and take in whether we are doing 3-link or 4-link setup and the nodal points?
-- do maths
