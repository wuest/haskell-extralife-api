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
    { message :: Maybe Text
    , createdOn :: !Time.UTCTime
    , donorName :: Maybe Text
    , avatarImageURL :: !String
    , donationAmount :: Float
    } deriving ( Show, Generic )
instance FromJSON Donation
instance ToJSON Donation
