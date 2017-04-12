{-# LANGUAGE DeriveGeneric #-}

module ExtraLife.Team where

import Data.Aeson   as Aeson
import Data.Time    as Time
import Data.Text             (Text)
import GHC.Generics          (Generic)
import Prelude

data Team = Team { totalRaisedAmount :: Float
                 , fundraisingGoal   :: Float
                 , createdOn         :: !Time.UTCTime
                 , avatarImageURL    :: !String
                 , teamID            :: Int
                 , name              :: !Text
                 } deriving ( Show, Generic )
instance FromJSON Team
instance ToJSON Team
