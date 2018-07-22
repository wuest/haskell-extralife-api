{-# LANGUAGE DeriveGeneric #-}

-- | Provides the 'User' type
--
-- Generally instantiated by 'Web.ExtraLife.API.userInfo'
module Web.ExtraLife.User where

import Prelude
import Data.Time    as Time
import Data.Text             (Text)
import GHC.Generics          (Generic)

import Data.Aeson   as Aeson

-- | Represents a user's information
data User = User
    { displayName     :: !Text
    , fundraisingGoal :: Float
    , participantID   :: Int
    , eventName       :: Text
    , avatarImageURL  :: !String
    , createdDateUTC  :: !Time.UTCTime
    , sumDonations    :: Float
    , numDonations    :: Int
    } deriving ( Show, Generic )
instance FromJSON User
instance ToJSON User
