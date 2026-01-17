{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import qualified Data.ByteString.Lazy as B
import Data.List (sortOn, sortBy, intercalate, subsequences)
import Data.Ord (comparing, Down(..))
import Control.Monad (guard, unless, when)
import GHC.Generics
import Data.Aeson
import System.IO.Unsafe (unsafePerformIO)
import System.Environment (getArgs)

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

data Course = Course 
    { code         :: !CourseCode
    , credits      :: !Int
    , prereqs      :: PrereqTree
    , availability :: ![Season]
    , requiresLab  :: Maybe CourseCode
    } deriving (Show, Eq, Ord, Generic)

data PlannerState = PlannerState
    { remainingReqs :: !(Set.Set Course)
    , completed     :: !(Set.Set CourseCode)
    , currentSem    :: !Semester
    , schedule      :: !(Map.Map Semester [Course])
    , totalCredits  :: !Int
    } deriving (Show, Eq, Ord)

instance FromJSON Course
instance FromJSON PrereqTree
instance FromJSON Season
instance ToJSON Course
instance ToJSON PrereqTree
instance ToJSON Season

loadCatalog :: FilePath -> IO (Set.Set Course)
loadCatalog path = do
    content <- B.readFile path
    case decode content of
        Just courses -> return (Set.fromList courses)
        Nothing      -> error "Failed to parse course catalog. Check JSON format."

-- ==========================================
-- CATALOG VALIDATION
-- ==========================================

-- | Check for courses that can never be scheduled
validateCatalog :: Set.Set Course -> IO ()
validateCatalog catalog = do
    let allCodes = Set.map code catalog
        issues = concatMap (checkCourse allCodes) (Set.toList catalog)
    
    unless (null issues) $ do
        putStrLn "WARNING: Found issues in catalog:"
        mapM_ putStrLn issues
        putStrLn ""
  where
    checkCourse allCodes c =
        let noAvailability = if null (availability c)
                            then ["  - " ++ code c ++ " has no availability (can never be scheduled)"]
                            else []
            missingPrereqs = findMissingPrereqs allCodes (code c) (prereqs c)
        in noAvailability ++ missingPrereqs
    
    findMissingPrereqs :: Set.Set CourseCode -> CourseCode -> PrereqTree -> [String]
    findMissingPrereqs allCodes courseName tree = case tree of
        None -> []
        CourseCode prereqCode -> 
            if prereqCode `Set.member` allCodes
            then []
            else ["  - " ++ courseName ++ " requires " ++ prereqCode ++ " which is not in catalog"]
        And trees -> concatMap (findMissingPrereqs allCodes courseName) trees
        Or trees -> concatMap (findMissingPrereqs allCodes courseName) trees
        CreditCount _ -> []

-- | Remove courses that can never be satisfied
cleanCatalog :: Set.Set Course -> Set.Set Course
cleanCatalog catalog =
    let allCodes = Set.map code catalog
        -- Remove courses with no availability
        withAvailability = Set.filter (not . null . availability) catalog
        -- Remove courses with missing prerequisites
        withValidPrereqs = Set.filter (hasAllPrereqs allCodes . prereqs) withAvailability
    in withValidPrereqs
  where
    hasAllPrereqs allCodes tree = case tree of
        None -> True
        CourseCode prereqCode -> prereqCode `Set.member` allCodes
        And trees -> all (hasAllPrereqs allCodes) trees
        Or trees -> any (hasAllPrereqs allCodes) trees  -- At least one branch must be satisfiable
        CreditCount _ -> True

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
-- HEURISTIC SCORING FUNCTIONS
-- ==========================================

-- | Calculate the maximum prerequisite depth of remaining courses
maxPrereqDepth :: Set.Set Course -> Set.Set CourseCode -> Int
maxPrereqDepth remaining completedCourses = 
    maximum (0 : map (prereqDepth completedCourses . prereqs) (Set.toList remaining))
  where
    prereqDepth _ None = 0
    prereqDepth comp (CourseCode c) = if c `Set.member` comp then 0 else 1
    prereqDepth comp (And trees) = maximum (0 : map (prereqDepth comp) trees)
    prereqDepth comp (Or trees) = minimum (map (prereqDepth comp) trees)
    prereqDepth _ (CreditCount _) = 0

-- | Count courses that could be taken this semester but weren't
missedOpportunities :: PlannerState -> [Course] -> Int
missedOpportunities state taken =
    let eligible = Set.filter isAvailable (remainingReqs state)
        isAvailable c = isEligible state (prereqs c) &&
                       season (currentSem state) `elem` availability c
        takenCodes = Set.fromList (map code taken)
        missed = Set.filter (\c -> code c `Set.notMember` takenCodes) eligible
    in Set.size missed

-- | Calculate workload variance across semesters (prefer balanced schedules)
workloadVariance :: Map.Map Semester [Course] -> Double
workloadVariance sched =
    let creditCounts = map (sum . map credits) (Map.elems sched)
        avg = fromIntegral (sum creditCounts) / fromIntegral (max 1 (length creditCounts))
        variance = sum [((fromIntegral c) - avg) ** 2 | c <- creditCounts] / fromIntegral (max 1 (length creditCounts))
    in variance

-- | Score a state (lower is better)
scoreState :: PlannerState -> Double
scoreState state
    | Set.null (remainingReqs state) = 0.0  -- Completed - best score
    | otherwise =
        let semesterCount = fromIntegral $ Map.size (schedule state)
            remainingCount = fromIntegral $ Set.size (remainingReqs state)
            depth = fromIntegral $ maxPrereqDepth (remainingReqs state) (completed state)
            variance = workloadVariance (schedule state)
        in semesterCount * 100           -- Minimize semesters (most important)
           + remainingCount * 10          -- Minimize remaining courses
           + depth * 5                    -- Minimize prerequisite bottlenecks
           + variance * 2                 -- Prefer balanced workload

-- ==========================================
-- COURSE COMBINATION GENERATION
-- ==========================================

-- | Generate valid course combinations (limited by maxCombos for beam search)
pickCombinations :: [Course] -> (Int, Int) -> Int -> Set.Set CourseCode -> [[Course]]
pickCombinations eligible (minC, maxC) maxCombos alreadyCompleted = 
    let allSubsets = subsequences eligible
        valid = do
            subset <- allSubsets
            let totalC = sum (map credits subset)
            guard $ totalC >= minC && totalC <= maxC
            guard $ all (hasRequiredLab subset) subset
            return subset
        -- Sort by credit total (prefer fuller schedules) and take top N
        sorted = sortBy (comparing (Down . sum . map credits)) valid
        result = take maxCombos sorted
    in result
  where
    hasRequiredLab selection c = case requiresLab c of
        Nothing      -> True
        Just labCode -> labCode `Set.member` alreadyCompleted ||  -- Already took it
                       any (\subC -> code subC == labCode) selection  -- Taking it now

-- ==========================================
-- BEAM SEARCH IMPLEMENTATION
-- ==========================================

nextSemester :: Semester -> Semester
nextSemester (Semester y Fall)   = Semester y Winter
nextSemester (Semester y Winter) = Semester y Spring
nextSemester (Semester y Spring) = Semester y Summer
nextSemester (Semester y Summer) = Semester (y + 1) Fall

-- | Beam search solver with configurable beam width
beamSearch :: Int -> PlannerState -> (Int, Int) -> [PlannerState]
beamSearch beamWidth initialState bounds = go [initialState] (0 :: Int)
  where
    maxDepth = 50  -- Increased safety limit
    
    go beam depth
        | depth > maxDepth = []
        | all (Set.null . remainingReqs) beam = beam  -- All complete
        | null beam = []
        | otherwise =
            let -- Generate successors for each state in beam
                successors = concatMap (expandState bounds) beam
                -- Score and sort all successors
                scored = sortBy (comparing scoreState) successors
                -- Keep only top beamWidth states
                nextBeam = take beamWidth scored
                -- Filter out completed states
                (completedStates, ongoing) = span (Set.null . remainingReqs) nextBeam
                
                -- Debug output every 5 semesters
                _ = if depth `mod` 5 == 0 && depth > 0
                    then unsafePerformIO $ putStrLn $ 
                         "Depth " ++ show depth ++ ": " ++ 
                         show (length beam) ++ " states, " ++
                         "best has " ++ show (Set.size $ remainingReqs $ head beam) ++ " courses left"
                    else ()
            in if null completedStates
               then go ongoing (depth + 1)
               else completedStates ++ go ongoing (depth + 1)

-- | Expand a single state into all possible next states
expandState :: (Int, Int) -> PlannerState -> [PlannerState]
expandState bounds state
    | Set.null (remainingReqs state) = [state]
    | otherwise =
        let -- First check basic eligibility (prereqs + availability)
            basicEligible = Set.filter isBasicAvailable (remainingReqs state)
            isBasicAvailable c = isEligible state (prereqs c) &&
                               season (currentSem state) `elem` availability c
            
            -- Then filter for lab requirements
            eligible = Set.filter (hasLabAvailable basicEligible) basicEligible
            hasLabAvailable eligSet c = case requiresLab c of
                Nothing -> True
                Just labCode -> labCode `Set.member` completed state ||  -- Already completed
                              any (\ec -> code ec == labCode) (Set.toList eligSet)  -- Available now
            
            eligibleList = Set.toList eligible
            
            -- Debug logging
            _ = if null eligibleList && not (Set.null (remainingReqs state))
                then unsafePerformIO $ do
                    putStrLn $ "  Warning: No eligible courses in " ++ 
                             show (season (currentSem state)) ++ " " ++ 
                             show (year (currentSem state))
                    putStrLn $ "    Remaining: " ++ show (Set.size (remainingReqs state)) ++ " courses"
                    putStrLn $ "    Basic eligible: " ++ show (Set.size basicEligible)
                    putStrLn $ "    After lab filter: " ++ show (length eligibleList)
                else ()
            
            -- Be more aggressive with combinations
            maxCombosPerState = 20
            combos = if null eligibleList
                     then [[]]  -- Skip this semester if no courses available
                     else pickCombinations eligibleList bounds maxCombosPerState (completed state)
            
        in map (applyCombo state) combos
  where
    applyCombo st courseCombo =
        let !newCompleted = completed st `Set.union` 
                           Set.fromList (map code courseCombo)
            !newRemaining = Set.filter (\c -> code c `Set.notMember` newCompleted) 
                                      (remainingReqs st)
            !newCredits = totalCredits st + sum (map credits courseCombo)
            !newSchedule = Map.insert (currentSem st) courseCombo (schedule st)
        in PlannerState
            { remainingReqs = newRemaining
            , completed     = newCompleted
            , currentSem    = nextSemester (currentSem st)
            , schedule      = newSchedule
            , totalCredits  = newCredits
            }

-- ==========================================
-- OUTPUT FORMATTING
-- ==========================================

formatSchedule :: Map.Map Semester [Course] -> String
formatSchedule sched =
    let sortedSemesters = sortOn fst (Map.toList sched)
        divider = replicate 60 '-'
        formatRow (sem, courses) =
            let header = "== " ++ show (season sem) ++ " " ++ show (year sem) ++ " =="
                courseList = if null courses
                             then "  (No courses scheduled)"
                             else intercalate "\n" $ map formatCourse courses
                total = "  Total Credits: " ++ show (sum $ map credits courses)
            in header ++ "\n" ++ courseList ++ "\n" ++ total
        formatCourse c = "  - " ++ code c ++ " (" ++ show (credits c) ++ " credits)"
    in intercalate ("\n" ++ divider ++ "\n") (map formatRow sortedSemesters)

formatSolution :: PlannerState -> String
formatSolution state =
    let totalSems = Map.size (schedule state)
        avgCredits = if totalSems > 0
                     then fromIntegral (totalCredits state) / fromIntegral totalSems
                     else 0.0 :: Double
        score = scoreState state
    in "Total Semesters: " ++ show totalSems ++
       " | Total Credits: " ++ show (totalCredits state) ++
       " | Avg Credits/Sem: " ++ show (round avgCredits :: Int) ++
       " | Score: " ++ show (round score :: Int)

-- ==========================================
-- CLI ARGUMENT PARSING
-- ==========================================

data Config = Config
    { catalogPath :: FilePath
    , beamWidth   :: Int
    , startYear   :: Int
    , startSeason :: Season
    , minCredits  :: Int
    , maxCredits  :: Int
    } deriving (Show)

defaultConfig :: FilePath -> Config
defaultConfig path = Config
    { catalogPath = path
    , beamWidth   = 5
    , startYear   = 2026
    , startSeason = Fall
    , minCredits  = 3
    , maxCredits  = 12
    }

parseArgs :: [String] -> Either String Config
parseArgs [] = Left "Missing required argument: catalog file"
parseArgs (path:rest) = parseOptions (defaultConfig path) rest
  where
    parseOptions cfg [] = Right cfg
    parseOptions cfg ("--beam":w:xs) = 
        case reads w of
            [(n, "")] | n > 0 -> parseOptions (cfg { beamWidth = n }) xs
            _ -> Left $ "Invalid beam width: " ++ w ++ " (must be a positive integer)"
    parseOptions cfg ("--year":y:xs) = 
        case reads y of
            [(n, "")] | n > 2000 && n < 2100 -> parseOptions (cfg { startYear = n }) xs
            _ -> Left $ "Invalid year: " ++ y ++ " (must be between 2000-2100)"
    parseOptions cfg ("--season":s:xs) = 
        case readSeason s of
            Just season -> parseOptions (cfg { startSeason = season }) xs
            Nothing -> Left $ "Invalid season: " ++ s ++ ". Use: Fall, Winter, Spring, or Summer"
    parseOptions cfg ("--min-credits":m:xs) = 
        case reads m of
            [(n, "")] | n >= 0 -> parseOptions (cfg { minCredits = n }) xs
            _ -> Left $ "Invalid min-credits: " ++ m ++ " (must be >= 0)"
    parseOptions cfg ("--max-credits":m:xs) = 
        case reads m of
            [(n, "")] | n > 0 -> parseOptions (cfg { maxCredits = n }) xs
            _ -> Left $ "Invalid max-credits: " ++ m ++ " (must be > 0)"
    parseOptions cfg ("--help":_) = Left helpText
    parseOptions cfg ("-h":_) = Left helpText
    parseOptions cfg (flag:_) | "--" `isPrefixOf` flag = 
        Left $ "Unknown option: " ++ flag ++ "\nUse --help for usage information"
    parseOptions _ (arg:_) = Left $ "Unexpected argument: " ++ arg
    
    isPrefixOf [] _ = True
    isPrefixOf _ [] = False
    isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys
    
    readSeason "Fall" = Just Fall
    readSeason "fall" = Just Fall
    readSeason "Winter" = Just Winter
    readSeason "winter" = Just Winter
    readSeason "Spring" = Just Spring
    readSeason "spring" = Just Spring
    readSeason "Summer" = Just Summer
    readSeason "summer" = Just Summer
    readSeason _ = Nothing
    
    helpText = unlines
        [ "Course Scheduler - Beam Search Planner"
        , ""
        , "Usage: planner <catalog.json> [OPTIONS]"
        , ""
        , "Required:"
        , "  <catalog.json>              Path to course catalog JSON file"
        , ""
        , "Options:"
        , "  --beam <width>              Beam search width (default: 5)"
        , "                              Higher = better quality but slower"
        , "  --year <year>               Starting year (default: 2026)"
        , "  --season <season>           Starting season: Fall, Winter, Spring, Summer"
        , "                              (default: Fall)"
        , "  --min-credits <credits>     Minimum credits per semester (default: 3)"
        , "  --max-credits <credits>     Maximum credits per semester (default: 12)"
        , "  -h, --help                  Show this help message"
        , ""
        , "Examples:"
        , "  planner catalog.json"
        , "  planner catalog.json --beam 10 --year 2025"
        , "  planner catalog.json --season Spring --min-credits 6 --max-credits 15"
        ]

-- ==========================================
-- MAIN
-- ==========================================

main :: IO ()
main = do
    args <- getArgs
    case parseArgs args of
        Left errMsg -> putStrLn errMsg
        Right config -> runPlanner config

runPlanner :: Config -> IO ()
runPlanner config = do
    putStrLn "Loading Catalog...\n"
    catalogRaw <- loadCatalog (catalogPath config)
    
    -- Validate and clean catalog
    validateCatalog catalogRaw
    let catalog = cleanCatalog catalogRaw
    
    when (Set.size catalog < Set.size catalogRaw) $ do
        putStrLn $ "Removed " ++ show (Set.size catalogRaw - Set.size catalog) ++ 
                   " unsatisfiable courses."
        putStrLn $ "Scheduling " ++ show (Set.size catalog) ++ " courses.\n"

    let initialState = PlannerState
           { remainingReqs = catalog
           , completed     = Set.empty
           , currentSem    = Semester (startYear config) (startSeason config)
           , schedule      = Map.empty
           , totalCredits  = 0
           }
    
    putStrLn "Running Beam Search...\n"
    putStrLn $ "Configuration:"
    putStrLn $ "  Start: " ++ show (startSeason config) ++ " " ++ show (startYear config)
    putStrLn $ "  Beam width: " ++ show (beamWidth config)
    putStrLn $ "  Credits per semester: " ++ show (minCredits config) ++ "-" ++ show (maxCredits config)
    putStrLn $ "  Total courses: " ++ show (Set.size catalog) ++ "\n"
    
    let bounds = (minCredits config, maxCredits config)
        results = beamSearch (beamWidth config) initialState bounds
    
    case results of
        [] -> do
            putStrLn $ "No solutions found with (" ++ show (minCredits config) ++ "," ++ 
                       show (maxCredits config) ++ ") credit range."
            putStrLn "Trying with relaxed constraints (0,18)...\n"
            let relaxedBounds = (0, max 18 (maxCredits config))
            let relaxedResults = beamSearch (beamWidth config * 2) initialState relaxedBounds
            case relaxedResults of
                [] -> putStrLn "Error: No valid schedule could be generated even with relaxed constraints."
                solutions -> displaySolutions solutions
        solutions -> displaySolutions solutions
  where
    displaySolutions solutions = do
        putStrLn $ "Found " ++ show (length solutions) ++ " solution(s):\n"
        
        -- Show top 3 solutions
        let topSolutions = take 3 solutions
        mapM_ (\(idx :: Int, sol) -> do
            putStrLn $ "\n========== Solution #" ++ show idx ++ " =========="
            putStrLn $ formatSolution sol
            putStrLn $ replicate 60 '='
            putStrLn $ formatSchedule (schedule sol)
            putStrLn ""
          ) (zip [1..] topSolutions)
