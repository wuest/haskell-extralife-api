{-# LANGUAGE OverloadedStrings #-}

-- | The ExtraLife API is extremely simple, providing interfaces to fetch user
-- information, user donations, team information and team membership lists.
-- All reference IDs are of type Int, and non-basic types are unnecessary to
-- represent the data returned from the API.
module Web.ExtraLife.API
    (
      -- * Interfaces to the remote API
      userInfo
    , recentDonations
    , teamInfo
    , teamMembers
    ) where

import Prelude

import Network.HTTP.Client     as HC
import Network.HTTP.Client.TLS as TLS
import Data.Aeson              as Aeson
import Data.ByteString.Lazy             ( ByteString )

import Web.ExtraLife.User                   ( User )
import Web.ExtraLife.Donation               ( Donation )
import Web.ExtraLife.Team                   ( Team )
import Web.ExtraLife.TeamMember             ( TeamMember )

-- Base for all URLS comprising the ExtraLife API
elRoot :: String
elRoot = "https://www.extra-life.org/api/"

-- Default HTTP Client settings reasonable for most use cases
httpSettings :: HC.ManagerSettings
httpSettings = HC.managerSetProxy (proxyEnvironment Nothing) tlsManagerSettings

-- Sends API call, returns the response
fetchFor :: Request -> IO ByteString
fetchFor req = do
    manager <- HC.newManager httpSettings
    response <- httpLbs req manager
    return $ responseBody response

userInfoRaw :: Int -> String
userInfoRaw = ((elRoot ++ "participants/") ++) . show

recentDonationsRaw :: Int -> String
recentDonationsRaw = (((elRoot ++ "participants/") ++) . (++ "/donations")) . show

teamInfoRaw :: Int -> String
teamInfoRaw = ((elRoot ++ "teams/") ++) . show

teamMembersRaw :: Int -> String
teamMembersRaw = (((elRoot ++ "teams/") ++) . (++ "/participants")) . show

userInfo' :: Int -> Request
userInfo' = parseRequest_ . userInfoRaw

recentDonations' :: Int -> Request
recentDonations' = parseRequest_ . recentDonationsRaw

teamInfo' :: Int -> Request
teamInfo' = parseRequest_ . teamInfoRaw

teamMembers' :: Int -> Request
teamMembers' = parseRequest_ . teamMembersRaw

-- | Fetches a given user's information, given a UID
userInfo :: Int -> IO (Maybe User)
userInfo u = do
    user <- fetchFor $ userInfo' u
    return (Aeson.decode user :: Maybe User)

-- | Fetches all recent donations for a given user
recentDonations :: Int -> IO (Maybe [Donation])
recentDonations u = do
    user <- fetchFor $ recentDonations' u
    return (Aeson.decode user :: Maybe [Donation])

-- | Fetches all information about a team except for members, given a Team ID
teamInfo :: Int -> IO (Maybe Team)
teamInfo t = do
    team <- fetchFor $ teamInfo' t
    return (Aeson.decode team :: Maybe Team)

-- | Fetches members of a team, if any, given a Team ID
teamMembers :: Int -> IO (Maybe [TeamMember])
teamMembers t = do
    team <- fetchFor $ teamMembers' t
    return (Aeson.decode team :: Maybe [TeamMember])
