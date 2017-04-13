{-# LANGUAGE DeriveGeneric #-}

module ExtraLife.Donation where

import Data.Aeson   as Aeson
import Data.Time    as Time
import Data.Text             (Text)
import GHC.Generics          (Generic)
import Prelude

data Donation = Donation
    { message :: Maybe Text
    , createdOn :: !Time.UTCTime
    , donorName :: Maybe Text
    , avatarImageURL :: !String
    , donationAmount :: Float
    } deriving ( Show, Generic )
instance FromJSON Donation
instance ToJSON Donation
