{-# LANGUAGE DeriveGeneric #-}

-- | Provides the 'Donation' type
--
-- Generally instantiated by 'Web.ExtraLife.API.recentDonations'
module Web.ExtraLife.Donation where

import Prelude
import Data.Time    as Time
import Data.Text             ( Text )
import GHC.Generics          ( Generic )

import Data.Aeson   as Aeson

-- | Represents a single donation made to a donation drive.
data Donation = Donation
    { displayName    :: Maybe Text
    , message        :: Maybe Text
    , participantID  :: Int
    , amount         :: Float
    , donorID        :: Text
    , createdDateUTC :: !Time.UTCTime
    , avatarImageURL :: !String
    } deriving ( Show, Generic )
instance FromJSON Donation
instance ToJSON Donation
