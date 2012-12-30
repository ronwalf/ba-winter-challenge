{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}
module Scoring (
  UserScore(..), 
  emptyScore,
  userScore,
  addScore,

  GroupScore(..),
  scoreGroup,
  groupScore,

  renderGroups,
  renderScores
) where

import Data.Data (Data)
import Data.Function (on)
import Data.List (sort)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import Data.Time.Calendar (Day, fromGregorian)
import Data.Time.Clock (utctDay)
import Data.Typeable
import Prelude as P
import Text.Blaze.Html5 hiding (map, head)
--import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes hiding (title, rows, accept, name, id)
--import qualified Text.Blaze.Html5.Attributes as A

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
} deriving (Data, Eq, Show, Typeable)

emptyScore :: Integer -> T.Text -> UserScore
emptyScore myuid n = UserScore myuid n 0 0 0 (fromGregorian 0 0 0) 0 0
userScore :: UserScore -> Int
userScore score = 10 * days score + floor (miles score)

instance Ord UserScore where
    compare = compare `on` (\u -> (userScore u, uid u))

addScore :: UserScore -> RideDetails -> UserScore
addScore score ride
    | currentDay score /= utctDay (rideStart ride)
        = addDay $ addMileage $ score { currentDay = utctDay $ rideStart ride, currentMiles = 0 }
    | otherwise
        = (if (currentMiles score < 1) then addDay else id) $ addMileage $ score
    where
    addMileage :: UserScore -> UserScore
    addMileage s = s { 
        rides = rides s + 1, 
        miles = miles s + rideMiles ride, 
        currentMiles = currentMiles s + rideMiles ride,
        currentRide = if (rideID ride > currentRide s) then rideID ride else currentRide s }
    addDay :: UserScore -> UserScore
    addDay s = if (currentMiles s >= 1) then s { days = days s + 1 } else s


data GroupScore = GroupScore {
    gid :: Integer,
    gname :: T.Text,
    gscores :: [UserScore]
} deriving (Data, Eq, Typeable)
groupScore :: GroupScore -> Int
groupScore = sum . P.map userScore . gscores

instance Ord GroupScore where
    compare = compare `on` (\g -> (groupScore g, gid g))

scoreGroup :: M.Map Integer UserScore -> ((T.Text, Integer), [(T.Text, Integer)]) -> GroupScore
scoreGroup scoreMap ((myname, gid'), members) = GroupScore gid' myname $
    mapMaybe (flip M.lookup scoreMap . snd) members


renderScores :: [UserScore] -> Html
{-
renderScores scores = table $ do
    tr $ do
        th ""
        th "Name"
        th "Points"
    mapM_ row $ zip [1..] $ reverse $ sort scores
    where
    row :: (Int, UserScore) -> Html
    row (points, us) = tr $ do
        td $ do
            toHtml points
            ": "
        td $ a ! href (toValue $ "http://app.strava.com/athletes/" ++ (show $ uid us)) $ toHtml $ name us
        td $ toHtml $ userScore us
-}
renderScores scores = ol $ do
    mapM_ row $ reverse $ sort scores
    where
    row us = li $ do
        a ! href (toValue $ "http://app.strava.com/athletes/" ++ (show $ uid us)) $ toHtml $ name us
        _ <- ": "
        td $ toHtml $ userScore us
        _ <- " points ("
        toHtml (days us)
        _ <- " days, "
        toHtml (floor $ miles us :: Int)
        " miles)"


renderGroups :: [GroupScore] -> Html
renderGroups groups = ol $ mapM_ groupHtml $ reverse $ sort groups
    where
    groupHtml :: GroupScore -> Html
    groupHtml gs = li $ do
        a ! href (toValue $ "http://app.strava.com/clubs/" ++ (show $ gid gs)) $ toHtml $ gname gs
        _ <- ": "
        toHtml (groupScore gs)
        _ <- " points"
        renderScores (gscores gs)

