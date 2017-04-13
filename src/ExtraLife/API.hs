{-# LANGUAGE OverloadedStrings #-}

module ExtraLife.API
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
import Data.ByteString.Lazy             (ByteString)

import ExtraLife.User                   (User)
import ExtraLife.Donation               (Donation)
import ExtraLife.Team                   (Team)
import ExtraLife.TeamMember             (TeamMember)

-- Base for all URLS comprising the ExtraLife API
elRoot :: String
elRoot = "https://www.extra-life.org/index.cfm?fuseaction=donorDrive."

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
userInfoRaw = (((elRoot ++ "participant&participantID=") ++) . (++ "&format=json")) . show

recentDonationsRaw :: Int -> String
recentDonationsRaw = (((elRoot ++ "participantDonations&participantID=") ++) . (++ "&format=json")) . show

teamInfoRaw :: Int -> String
teamInfoRaw = (((elRoot ++ "team&teamID=") ++) . (++ "&format=json")) . show

teamMembersRaw :: Int -> String
teamMembersRaw = (((elRoot ++ "teamParticipants&teamID=") ++) . (++ "&format=json")) . show

userInfo' :: Int -> Request
userInfo' = parseRequest_ . userInfoRaw

recentDonations' :: Int -> Request
recentDonations' = parseRequest_ . recentDonationsRaw

teamInfo' :: Int -> Request
teamInfo' = parseRequest_ . teamInfoRaw

teamMembers' :: Int -> Request
teamMembers' = parseRequest_ . teamMembersRaw

userInfo :: Int -> IO (Maybe User)
userInfo u = do
    user <- fetchFor $ userInfo' u
    return (Aeson.decode user :: Maybe User)

recentDonations :: Int -> IO (Maybe [Donation])
recentDonations u = do
    user <- fetchFor $ recentDonations' u
    return (Aeson.decode user :: Maybe [Donation])

teamInfo :: Int -> IO (Maybe Team)
teamInfo t = do
    team <- fetchFor $ teamInfo' t
    return (Aeson.decode team :: Maybe Team)

teamMembers :: Int -> IO (Maybe [TeamMember])
teamMembers t = do
    team <- fetchFor $ teamMembers' t
    return (Aeson.decode team :: Maybe [TeamMember])
