{-# LANGUAGE OverloadedStrings #-}

import Technique.Procedure

main :: IO ()
main = do
  result <- runProcedure $ example
  print result

strike :: Procedure Int
strike = do
  issueLaunchCodes
  submarine codes <> bomber codes
  countImpacts

submarine :: LaunchCodes -> Procedure Int
submarine = do
  armWarheads
  fuelMissiles
  launchMissiles

bomber :: LaunchCodes -> Procedure Int
bomber = do
  armWarheads
  openBayDoors
  dropBombs

armWarheads :: Procedure ()
armWarheads = Procedure

launchMissiles = undefined

countImpacts = undefined
