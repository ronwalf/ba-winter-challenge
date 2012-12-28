{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module Main where

import Control.Applicative ((<$>), (<*>))
import Control.Failure (Failure)
import Control.Monad (liftM)
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Data.Conduit (MonadBaseControl, MonadResource)
import Data.Function (on)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import Data.List (foldl', nub, sort, sortBy)
import qualified Data.Text as T
import Data.Time.Calendar (Day, fromGregorian)
--import Data.Time.Clock (UTCTime, utctDay)
import Data.Time.Format (parseTime)
import Network (withSocketsDo)
import Network.HTTP.Conduit (withManager, Manager, HttpException)
import System.Console.GetOpt
import System.Environment (getArgs)
import System.Locale (defaultTimeLocale)


import qualified Scoring as S
import Strava

main = withSocketsDo $ do
    argv <- getArgs
    (opts, config) <- case getOpt Permute options argv of
        (o, [cfname], []) -> do
            cf <- B.readFile cfname
            case decode' cf of
                Just c -> return (o, c)
                Nothing -> fail "Could not parse config file"
        (_, _, errs) ->
            ioError $ userError $
            concat errs ++ usageInfo "Usage: ba-winter-challenge [OPTION..] config-file" options
    (groups, rides) <- withManager $ \manager -> do
        groups <- mapM (flip groupMembers manager) $ (challengeGroups config ++ challengeOthers config)
        let members = nub $ sortBy (compare `on` snd) $ concatMap snd groups
        rides <- mapM (\m@(_, uid) -> liftM (\x -> (m, x)) $ userRides uid (challengeStart config) (challengeEnd config) manager) members
        return (filter (flip elem (challengeGroups config) . snd . fst) groups, rides)
    putStrLn "Collating..."
    let loadedScores = M.empty
    let scoreMap = foldl' addRides loadedScores rides
    let groupScores = sort $ map (scoreGroup scoreMap) groups
    let userScores = sort $ map snd $ M.toList scoreMap
    putStrLn "Group Scores:"
    mapM_ (\g -> print (gname g, groupScore g)) groupScores
    putStrLn "User Scores:"
    mapM_ (\u -> print (S.name u, S.userScore u)) userScores
    return ()
    where
    addRides :: M.Map Integer S.UserScore -> ((T.Text, Integer), [RideDetails]) -> M.Map Integer S.UserScore
    addRides scoreMap ((name, uid), rides) = score `seq` M.insert uid score scoreMap
        where
        score = foldl' S.addScore (M.findWithDefault (S.emptyScore uid name) uid scoreMap) rides

data Options = Options { clearCache :: Bool }
defaultOpts :: Options
defaultOpts = Options { clearCache = False }

options :: [OptDescr (Options -> Options)]
options = 
    [ Option ['c'] ["clear"] 
        (NoArg (\opts -> opts { clearCache = True }))
        "Clear score cache"
    ]


data ChallengeConfig = ChallengeConfig {
    challengeDir :: String,
    challengeGroups :: [Integer],
    challengeOthers :: [Integer],
    challengeStart :: Day,
    challengeEnd :: Day 
} deriving Show

instance FromJSON ChallengeConfig where
    parseJSON (Object v) = ChallengeConfig <$>
        v .: "directory" <*>
        v .: "groups" <*>
        v .: "others" <*>
        v .: "start" <*>
        v .: "end"
    parseJSON _ = fail "Expected object to parse challenge config data"

instance FromJSON Day where
    parseJSON (String t) = case parseTime defaultTimeLocale "%F" (T.unpack t) of
        Just d -> return d
        _ -> fail "could not parse day"
    parseJSON _ = fail "Expecting string when parsing a Day"

userRides :: (Failure HttpException m, MonadBaseControl IO m, MonadResource m) => Integer -> Day -> Day -> Manager -> m [RideDetails]
userRides uid start end manager = do
    urides <- rideSearch (rsDefault { userID = Just uid, startDay = Just start, endDay = Just end }) manager
    liftM sort $ mapM (liftM snd . (flip rideDetails manager) . snd) urides


data GroupScore = GroupScore {
    gid :: Integer,
    gname :: T.Text,
    gscores :: [S.UserScore]
} deriving Eq
groupScore :: GroupScore -> Int
groupScore = sum . map S.userScore . gscores

instance Ord GroupScore where
    compare = compare `on` (\g -> (groupScore g, gid g))

scoreGroup :: M.Map Integer S.UserScore -> ((T.Text, Integer), [(T.Text, Integer)]) -> GroupScore
scoreGroup scoreMap ((name, gid'), members) = GroupScore gid' name $
    mapMaybe (flip M.lookup scoreMap . snd) members




