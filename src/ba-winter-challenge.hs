{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, OverloadedStrings #-}
module Main where

import Control.Applicative ((<$>), (<*>))
import Control.Failure (Failure)
import Control.Monad (liftM, when)
import Data.Aeson
import qualified Data.Aeson.Generic as AG
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as B
import Data.Conduit (MonadBaseControl, MonadResource)
import Data.Data (Data)
import Data.Function (on)
import Data.List (foldl', nub, sort, sortBy)
import qualified Data.Map as M
--import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Time.Calendar (Day)
import Data.Time.LocalTime (ZonedTime, getZonedTime)
import Data.Time.Format (parseTime, formatTime)
import Data.Typeable (Typeable)
import Network (withSocketsDo)
import Network.HTTP.Conduit (withManager, Manager, HttpException)
import System.Console.GetOpt
import System.Directory (createDirectory, doesDirectoryExist)
import System.Environment (getArgs)
import System.FilePath (combine)
import System.Locale (defaultTimeLocale)
import Text.Blaze.Html.Renderer.Utf8
import Text.Blaze.Html5 hiding (map, head)
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes hiding (title, rows, accept, name, dir, start)
--import qualified Text.Blaze.Html5.Attributes as A


import qualified Scoring as S
import Strava

main :: IO ()
main = withSocketsDo $ do
    argv <- getArgs
    (opts, config) <- case getOpt Permute options argv of
        (o, [cfname], []) -> do
            cf <- B.readFile cfname
            case decode' cf of
                Just c -> return (foldl' (\opts f -> f opts) defaultOpts o, c)
                Nothing -> fail "Could not parse config file"
        (_, _, errs) ->
            ioError $ userError $
            concat errs ++ usageInfo "Usage: ba-winter-challenge [OPTION..] config-file" options
    loadedScores <- if (clearCache opts)
        then return M.empty
        else loadScores config
    (groups, rides) <- withManager $ \manager -> do
        groups <- mapM (flip groupMembers manager) $ (challengeGroups config ++ challengeOthers config)
        let members = nub $ sortBy (compare `on` snd) $ concatMap snd groups
        rides <- mapM (\m@(_, uid) -> liftM (\x -> (m, filter (rideFilters config) x)) $ userRides uid (challengeStart config) (challengeEnd config) (1 + (maybe 0 S.currentRide $ M.lookup uid loadedScores)) manager) members
        return (filter (flip elem (challengeGroups config) . snd . fst) groups, rides)
    -- putStrLn "Collating..."
    ensureDirectory (challengeDir config)
    let scoreMap = foldl' addRides loadedScores rides
    saveScores config scoreMap
    let groupScores = sort $ map (S.scoreGroup scoreMap) groups
    let userScores = sort $ map snd $ M.toList scoreMap
    -- putStrLn "Group Scores:"
    -- mapM_ (\g -> print (S.gname g, S.groupScore g)) groupScores
    -- putStrLn "User Scores:"
    -- mapM_ (\u -> print (S.name u, S.userScore u)) userScores
    ztime <- getZonedTime
    B.writeFile (combine (challengeDir config) "index.html") $ renderHtml $ render config ztime userScores groupScores
    return ()
    where
    addRides :: M.Map Integer S.UserScore -> ((T.Text, Integer), [RideDetails]) -> M.Map Integer S.UserScore
    addRides scoreMap ((name, uid), rides) = score `seq` M.insert uid score scoreMap
        where
        score = foldl' S.addScore (M.findWithDefault (S.emptyScore uid name) uid scoreMap) rides
    ensureDirectory :: String -> IO ()
    ensureDirectory dir = do
        exists <- doesDirectoryExist dir
        when (not exists) $ do
            createDirectory dir
        return ()
    loadScores :: ChallengeConfig -> IO (M.Map Integer S.UserScore)
    loadScores config = do
        scoreFile <- BS.readFile (combine (challengeDir config) "scores.json")
        return $ maybe M.empty toScoreMap $ AG.decode' $ B.pack $ BS.unpack $ scoreFile
    saveScores config scoreMap = B.writeFile (combine (challengeDir config) "scores.json") $ AG.encode $ fromScoreMap scoreMap
    rideFilters :: ChallengeConfig -> RideDetails -> Bool
    rideFilters config = not . or . flip map (map T.toCaseFold $ challengeFilters config) . (\r -> flip T.isInfixOf (T.toCaseFold (rideName r)))

data Options = Options { clearCache :: Bool }
defaultOpts :: Options
defaultOpts = Options { clearCache = False }

options :: [OptDescr (Options -> Options)]
options = 
    [ Option ['c'] ["clear"] 
        (NoArg (\opts -> opts { clearCache = True }))
        "Clear score cache"
    ]


data ScoreFile = ScoreFile [S.UserScore] deriving (Data, Typeable)
fromScoreMap :: M.Map Integer S.UserScore -> ScoreFile
fromScoreMap = ScoreFile . map snd . M.toList
toScoreMap :: ScoreFile -> M.Map Integer S.UserScore
toScoreMap (ScoreFile scores) = M.fromList $ map (\score -> (S.uid score, score)) scores


data ChallengeConfig = ChallengeConfig {
    challengeTitle :: T.Text,
    challengeDesc :: T.Text,
    challengeDir :: String,
    challengeFilters :: [T.Text],
    challengeGroups :: [Integer],
    challengeOthers :: [Integer],
    challengeStart :: Day,
    challengeEnd :: Day 
} deriving Show


instance FromJSON ChallengeConfig where
    parseJSON (Object v) = ChallengeConfig <$>
        v .: "title" <*>
        v .: "desc" <*>
        v .: "directory" <*>
        v .: "filters" <*>
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

userRides :: (Failure HttpException m, MonadBaseControl IO m, MonadResource m) => Integer -> Day -> Day -> Integer -> Manager -> m [RideDetails]
userRides uid start end sid manager = do
    urides <- rideSearch (rsDefault { userID = Just uid, startDay = Just start, endDay = Just end, startId = Just sid }) manager
    liftM sort $ mapM (liftM snd . (flip rideDetails manager) . snd) urides


render :: ChallengeConfig -> ZonedTime -> [S.UserScore] -> [S.GroupScore] -> Html
render config ztime userScores groupScores = docTypeHtml $ do
    H.head $ do
        title (toHtml $ challengeTitle config)
        link ! rel "stylesheet" ! href "style.css" ! type_ "text/css"
    body $ do
        h1 (toHtml $ challengeTitle config)
        preEscapedToHtml $ challengeDesc config
        h2 "Group Scores"
        S.renderGroups groupScores
        h2 "Individual Scores"
        S.renderScores userScores
        h2 "Total Stats"
        ul $ do
            li $ do
                toHtml $ sum $ map S.rides userScores
                " rides"
            li $ do
                toHtml $ (floor :: Double -> Int) $ sum $ map S.miles userScores
                " miles"
            li $ do
                toHtml $ sum $ map S.userScore userScores
                " total points earned"
        p $ do
            _ <- "Last update at "
            toHtml $ formatTime defaultTimeLocale "%T on %D" ztime
        p $ a ! href "https://github.com/ronwalf/ba-winter-challenge" $ "Get the BA-Winter Challenge Code!"
            
