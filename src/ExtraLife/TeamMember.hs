{-# LANGUAGE DeriveGeneric #-}

-- | Provides the 'TeamMember' type; note that this has an additional field
-- compared to the 'User' type, 'isTeamCaptain' and lacks much of the context
-- provided by the 'User' type.
--
-- Generally instantiated by 'ExtraLife.API.teamMembers' (returned as a List)
module ExtraLife.TeamMember where

import Prelude
import Data.Time    as Time
import Data.Text             ( Text )
import GHC.Generics          ( Generic )

import Data.Aeson   as Aeson

-- | Represents a single member of a given team
data TeamMember =
    TeamMember { displayName    :: !Text
               , participantID  :: Int
               , createdOn      :: !Time.UTCTime
               , avatarImageURL :: !String
               , isTeamCaptain  :: Bool
               } deriving ( Show, Generic )
instance FromJSON TeamMember
instance ToJSON TeamMember
