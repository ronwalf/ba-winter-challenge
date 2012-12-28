{-# LANGUAGE OverloadedStrings #-}
module Scoring (
  UserScore(..), 
  emptyScore,
  userScore,
  addScore
) where

import Data.Function (on)
import qualified Data.Text as T
import Data.Time.Calendar (Day, fromGregorian)
import Data.Time.Clock (utctDay)


import Strava (RideDetails(..))

rideMiles :: RideDetails -> Double
rideMiles = (*0.00062137119) . rideDistance

data UserScore = UserScore {
    uid :: !Integer,
    name :: !T.Text,
    miles :: !Double,
    days :: !Int,
    rides :: !Int,
    currentDay :: !Day,
    currentMiles :: !Double,
    currentRide :: !Integer
} deriving (Eq, Show)

emptyScore :: Integer -> T.Text -> UserScore
emptyScore uid n = UserScore uid n 0 0 0 (fromGregorian 0 0 0) 0 0
userScore :: UserScore -> Int
userScore score = 10 * days score + floor (miles score)

instance Ord UserScore where
    compare = compare `on` (\u -> (userScore u, uid u))

addScore :: UserScore -> RideDetails -> UserScore
addScore score ride
    | currentDay score /= utctDay (rideStart ride)
        = updateDays $ addMileage $ score { currentDay = utctDay $ rideStart ride, currentMiles = 0 }
    | otherwise
        = updateDays $ addMileage $ score
    where
    addMileage :: UserScore -> UserScore
    addMileage s = s { 
        rides = rides s + 1, 
        miles = miles s + rideMiles ride, 
        currentMiles = currentMiles s + rideMiles ride,
        currentRide = rideID ride}
    updateDays score' =
        if (currentMiles score < 1 && currentMiles score' > 1)
            then score' { days = days score' + 1 }
            else score'
