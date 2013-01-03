{-# LANGUAGE FlexibleContexts, FlexibleInstances, OverloadedStrings #-}
module Strava (
    groupMembers,
    rideSearch, RSOpts(..), rsDefault,
    rideDetails, RideDetails(..)
) where

import Control.Applicative ((<$>), (<*>))
import Control.Failure (Failure)
import Control.Monad (liftM)
import Data.Aeson
import Data.Conduit (MonadBaseControl, MonadResource)
import Data.Function (on)
import Data.List (intercalate)
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import Data.Time.Calendar (Day)
import Data.Time.Clock (UTCTime)
import Data.Time.Format (formatTime)
import Network.HTTP.Conduit

import System.Locale (defaultTimeLocale)

groupMembers :: (Failure HttpException m, MonadBaseControl IO m, MonadResource m) => Integer -> Manager -> m ((T.Text, Integer), [(T.Text, Integer)])
groupMembers gNum manager = do
    gl <- getJSON ("http://www.strava.com/api/v1/clubs/" ++ show gNum ++ "/members") manager
    return $ fromGroupList gl


rideSearch :: (Failure HttpException m, MonadBaseControl IO m, MonadResource m) => RSOpts -> Manager -> m [(T.Text, Integer)]
rideSearch opts manager = do
    retrieveAll 0 
    where
    
    retrieveAll offset = do
        gl <- liftM fromRideList $ flip getJSON manager $ "http://www.strava.com/api/v1/rides?" ++ intercalate "&" (catMaybes [
                fmap (renderNum "clubId") $ clubID opts,
                fmap (renderNum "athleteId") $ userID opts,
                fmap (renderDay "startDate") $ startDay opts,
                fmap (renderDay "endDate") $ endDay opts,
                fmap (renderNum "startId") $ startId opts,
                if (offset > 0) then Just (renderNum "offset" offset) else Nothing
                ])
        let glLen = length gl
        if glLen >= 50 
            then do
                gl' <- retrieveAll (offset + glLen)
                return (gl ++ gl')
            else return gl
    renderNum var num = var ++ "=" ++ show num
    renderDay var day = var ++ "=" ++ formatTime defaultTimeLocale "%F" day

data RSOpts = RSOpts {
    userID :: Maybe Integer,
    clubID :: Maybe Integer,
    startDay :: Maybe Day,
    endDay :: Maybe Day,
    startId :: Maybe Integer
}
rsDefault :: RSOpts
rsDefault = RSOpts Nothing Nothing Nothing Nothing Nothing


rideDetails :: (Failure HttpException m, MonadBaseControl IO m, MonadResource m) => Integer -> Manager -> m ((T.Text, T.Text), RideDetails)
rideDetails rid manager = getJSON ("http://app.strava.com/api/v2/rides/" ++ (show rid)) manager


getJSON :: (FromJSON a, Failure HttpException m, MonadBaseControl IO m, MonadResource m) => String -> Manager -> m a
getJSON urlString manager = do
    --response <- simpleHTTP (getRequest_ urlString)
    req <- parseUrl urlString
    body <- httpLbs req manager
    case decode' (responseBody body) of
        Just a -> return a
        Nothing -> fail $ "Could not decode body: " ++ show body



newtype NameID = NameID (T.Text, Integer)
fromNameID :: NameID -> (T.Text, Integer)
fromNameID (NameID (nt, nid)) = (nt, nid)

instance FromJSON NameID where
    parseJSON (Object v) = do
        nameText <- v .: "name"
        nameID <- v .: "id"
        return $ NameID (nameText, nameID)
    parseJSON _ = fail "Expecting object to parse name and id"

newtype GroupList = GroupList (NameID, [NameID])
fromGroupList :: GroupList -> ((T.Text, Integer), [(T.Text, Integer)])
fromGroupList (GroupList (nid, gl)) = (fromNameID nid, map fromNameID gl)
instance FromJSON GroupList where
    parseJSON (Object v) = do
        nameID <- v .: "club"
        members <- v .: "members"
        return $ GroupList (nameID, members)
    parseJSON _ = fail "Expecting object to parse name and id"


newtype RideList = RideList [NameID]
fromRideList :: RideList -> [(T.Text, Integer)]
fromRideList (RideList rl) = map fromNameID rl
instance FromJSON RideList where
    parseJSON (Object v) = do
        rides <- v .: "rides"
        return $ RideList rides
    parseJSON _ = fail "Expecting object to parse ride list"

data RideDetails = RideDetails {
    rideID :: Integer,
    rideName :: T.Text,
    rideStart :: UTCTime,
    rideElapsedTime :: Int,
    rideMovingTime :: Int,
    rideDistance :: Double,
    rideAvgSpeed :: Double,
    rideElevation :: Double,
    rideLocation :: Maybe T.Text
} deriving (Show, Eq)


instance Ord RideDetails where
    compare = compare `on` rideStart

instance FromJSON ((T.Text, T.Text), RideDetails) where
    parseJSON (Object v) = do
        rid <- v .: "id"
        ver <- v .: "version"
        details <- v .: "ride"
        return ((rid, ver), details)
    parseJSON _ = fail "Expecting object to parse ride response"
instance FromJSON RideDetails where
    parseJSON (Object v) = RideDetails <$>
            v .: "id" <*>
            v .: "name" <*>
            v .: "start_date_local" <*>
            v .: "elapsed_time" <*>
            v .: "moving_time" <*>
            v .: "distance" <*>
            v .: "average_speed" <*>
            v .: "elevation_gain" <*>
            v .:? "location"
    parseJSON _ = fail "Expecting object to parse ride details" 
