{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as B
import Data.List (subsequences, sortOn, intercalate)
import Control.Monad (guard)
import GHC.Generics
import Data.Aeson
import System.Environment (getArgs)

-- ==========================================
-- 1. DATA TYPES
-- ==========================================

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
    deriving (Show, Eq, Generic)

data Course = Course { code         :: CourseCode
    , credits      :: Int
    , prereqs      :: PrereqTree
    , availability :: [Season]
    , requiresLab  :: Maybe CourseCode -- Co-requisite logic
    } deriving (Show, Eq, Generic)

data PlannerState = PlannerState
    { remainingReqs :: [Course]
    , completed     :: Set.Set CourseCode
    , currentSem    :: Semester
    , schedule      :: Map.Map Semester [Course]
    , totalCredits  :: Int
    } deriving (Show)

instance FromJSON Course
instance FromJSON PrereqTree
instance FromJSON Season
instance ToJSON Course
instance ToJSON PrereqTree
instance ToJSON Season

loadCatalog :: FilePath -> IO [Course]
loadCatalog path = do
    content <- B.readFile path
    case decode content of
        Just courses -> return courses
        Nothing      -> error "Failed to parse course catalog. Check JSON format."

-- ==========================================
-- 2. EVALUATOR LOGIC
-- ==========================================

isEligible :: PlannerState -> PrereqTree -> Bool
isEligible _ None = True
isEligible state (CourseCode cCode) = 
    cCode `Set.member` completed state
isEligible state (And reqs) = 
    all (isEligible state) reqs
isEligible state (Or reqs) = 
    any (isEligible state) reqs
isEligible state (CreditCount n) = 
    totalCredits state >= n

-- ==========================================
-- 3. COMBINATION & SOLVER LOGIC
-- ==========================================

-- | Returns all valid subsets of courses for a single semester
pickValidCombination :: [Course] -> (Int, Int) -> [[Course]]
pickValidCombination eligible (minC, maxC) = do
    subset <- subsequences eligible
    let totalC = sum (map credits subset)
    guard $ totalC >= minC && totalC <= maxC
    guard $ all (hasRequiredLab subset) subset
    return subset
  where
    hasRequiredLab selection c = case requiresLab c of
        Nothing      -> True
        Just labCode -> any (\subC -> code subC == labCode) selection

-- | Logic to progress time
nextSemester :: Semester -> Semester
nextSemester (Semester y Summer) = Semester (y + 1) Fall
nextSemester (Semester y s)      = Semester y (succ s)

-- | The core recursive solver
solve :: PlannerState -> (Int, Int) -> [Map.Map Semester [Course]]
solve state bounds
    | null (remainingReqs state) = return (schedule state)
    | year (currentSem state) > 2034 = [] -- Safety break to prevent infinite loops
    | otherwise = do
        -- 1. Find courses where prereqs are met and it's offered this season
        let eligible = filter (\c -> isEligible state (prereqs c) && 
                                    season (currentSem state) `elem` availability c) 
                              (remainingReqs state)
        
        -- 2. Pick a combo (including an empty one if no courses are eligible/needed)
        courseCombo <- pickValidCombination eligible bounds
        
        -- 3. Update state and recurse
        let newCompleted = completed state `Set.union` Set.fromList (map code courseCombo)
        let nextState = state 
                { remainingReqs = filter (\c -> code c `Set.notMember` newCompleted) (remainingReqs state)
                , completed     = newCompleted
                , currentSem    = nextSemester (currentSem state)
                , schedule      = Map.insert (currentSem state) courseCombo (schedule state)
                , totalCredits  = totalCredits state + sum (map credits courseCombo)
                }
        solve nextState bounds

-- | Converts the resulting Map into a formatted string
formatSchedule :: Map.Map Semester [Course] -> String
formatSchedule sched = 
    let sortedSemesters = sortOn fst (Map.toList sched)
        divider = replicate 50 '-'
        formatRow (sem, courses) = 
            let header = "== " ++ show (season sem) ++ " " ++ show (year sem) ++ " =="
                courseList = if null courses 
                             then "  (No courses scheduled)"
                             else intercalate "\n" $ map formatCourse courses
                total = "  Total Credits: " ++ show (sum $ map credits courses)
            in header ++ "\n" ++ courseList ++ "\n" ++ total
        formatCourse c = "  - " ++ code c ++ " (" ++ show (credits c) ++ " units)"
    in intercalate ("\n" ++ divider ++ "\n") (map formatRow sortedSemesters)

main :: IO ()
main = do
    args <- getArgs
    case args of
      [filePath] -> do
        putStrLn "Loading Catalog...\n"
        catalog <- loadCatalog filePath

        let initialState = PlannerState 
               { remainingReqs = catalog
               , completed     = Set.empty
               , currentSem    = Semester 2026 Fall
               , schedule      = Map.empty
               , totalCredits  = 0
               }
        putStrLn "Generating Course Plan...\n"
        let results = solve initialState (3, 12)
        case results of
            []    -> putStrLn "Error: No valid schedule could be generated with the given constraints."
            (s:_) -> do
                putStrLn "Suggested Academic Plan:"
                putStrLn "=================================================="
                putStrLn $ formatSchedule s
                putStrLn "=================================================="
                putStrLn $ "Total Semesters: " ++ show (Map.size s)
      [] -> putStrLn "Error"
      _ -> putStrLn "Error"
