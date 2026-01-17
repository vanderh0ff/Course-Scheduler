{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Scheduler.Types where

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import GHC.Generics
import Data.Aeson
import Data.Aeson.Types (Parser)
import Control.Applicative ((<|>))

type CourseCode = String

data Season = Fall | Spring | Summer | Winter
    deriving (Eq, Ord, Enum, Show, Read, Generic)

data Semester = Semester
    { year   :: Int
    , season :: Season
    } deriving (Eq, Ord, Show)

data PrereqTree
    = None
    | CourseCode CourseCode
    | And [PrereqTree]
    | Or [PrereqTree]
    | CreditCount Int
    deriving (Show, Eq, Ord, Generic)

data Requirement
    = CourseReq Course           -- A specific course dependency
    | OneOf [Requirement]        -- Satisfy exactly one branch
    | AllOf [Requirement]        -- Satisfy all branches (grouping)
    | CreditReq Int              -- Need N credits (placeholder for now/future)
    deriving (Show, Eq, Ord, Generic)

data Course = Course 
    { code         :: !CourseCode
    , credits      :: !Int
    , prereqs      :: PrereqTree
    , availability :: ![Season]
    , requiresLab  :: Maybe CourseCode
    } deriving (Show, Eq, Ord, Generic)

data PlannerState = PlannerState
    { remainingReqs :: ![Requirement]
    , completed     :: !(Set.Set CourseCode)
    , currentSem    :: !Semester
    , schedule      :: !(Map.Map Semester [Course])
    , totalCredits  :: !Int
    } deriving (Show, Eq, Ord)

data Config = Config
    { catalogPath :: FilePath
    , beamWidth   :: Int
    , startYear   :: Int
    , startSeason :: Season
    , minCreditsPerSeason :: Map.Map Season Int
    , maxCreditsPerSeason :: Map.Map Season Int
    } deriving (Show)

instance FromJSON Course
instance FromJSON PrereqTree
instance FromJSON Season
instance ToJSON Course
instance ToJSON PrereqTree
instance ToJSON Season

-- Custom JSON parsing for Requirement to allow mixed list in catalog
instance FromJSON Requirement where
    parseJSON v = 
        -- Try parsing as a Course first
        (CourseReq <$> parseJSON v)
        -- Then try structured requirements
        <|> (withObject "Requirement" $ \o -> do
            tag <- o .: "tag" :: Parser String
            case tag of
                "OneOf" -> OneOf <$> o .: "contents"
                "AllOf" -> AllOf <$> o .: "contents"
                _       -> fail "Unknown requirement tag"
            ) v

instance ToJSON Requirement
