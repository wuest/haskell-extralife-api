{-# LANGUAGE DeriveGeneric #-}

module ExtraLife.User where

import Data.Aeson   as Aeson
import Data.Time    as Time
import Data.Text             (Text)
import GHC.Generics          (Generic)
import Prelude

data User = User { displayName       :: !Text
                 , totalRaisedAmount :: Float
                 , fundraisingGoal   :: Float
                 , participantID     :: Int
                 , createdOn         :: !Time.UTCTime
                 , avatarImageURL    :: !String
                 , teamID            :: Maybe Int
                 , isTeamCaptain     :: Bool
                 } deriving ( Show, Generic )
instance FromJSON User
instance ToJSON User
