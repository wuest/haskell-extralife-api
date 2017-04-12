{-# LANGUAGE DeriveGeneric #-}

module ExtraLife.TeamMember where

import Data.Aeson   as Aeson
import Data.Time    as Time
import Data.Text             (Text)
import GHC.Generics          (Generic)
import Prelude

data TeamMember =
    TeamMember { displayName    :: !Text
               , participantID  :: Int
               , createdOn      :: !Time.UTCTime
               , avatarImageURL :: !String
               , isTeamCaptain  :: Bool
               } deriving ( Show, Generic )
instance FromJSON TeamMember
instance ToJSON TeamMember
