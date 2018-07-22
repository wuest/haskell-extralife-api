{-# LANGUAGE DeriveGeneric #-}

-- | Provides the 'Team' type
--
-- Generally instantiated by 'Web.ExtraLife.API.teamInfo'
module Web.ExtraLife.Team where

import Prelude
import Data.Time    as Time
import Data.Text             ( Text )
import GHC.Generics          ( Generic )

import Data.Aeson   as Aeson

-- | Represents a team of donation drive participants
data Team = Team
    { fundraisingGoal :: Float
    , eventName       :: Text
    , avatarImageURL  :: !String
    , createdDateUTC  :: !Time.UTCTime
    , sumDonations    :: Float
    , teamID          :: Int
    , name            :: !Text
    , numDonations    :: Int
    } deriving ( Show, Generic )
instance FromJSON Team
instance ToJSON Team
