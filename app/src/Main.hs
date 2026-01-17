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
import Control.Applicative ((<|>))
import GHC.Generics
import Data.Aeson
import Data.Aeson.Types (Parser)
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
    { remainingReqs :: ![Requirement]  -- CHANGED: Set Course -> [Requirement]
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

loadCatalog :: FilePath -> IO [Requirement]
loadCatalog path = do
    content <- B.readFile path
    case decode content of
        Just reqs -> return reqs
        Nothing      -> error "Failed to parse course catalog. Check JSON format."

-- ==========================================
-- CATALOG VALIDATION
-- ==========================================

-- ==========================================
-- REQUIREMENT LOGIC
-- ==========================================

-- | Flatten requirements to get all involved courses (for catalog validation/stats)
getAllCourses :: [Requirement] -> Set.Set Course
getAllCourses reqs = Set.unions (map getReqCourses reqs)
  where
    getReqCourses (CourseReq c) = Set.singleton c
    getReqCourses (OneOf rs) = getAllCourses rs
    getReqCourses (AllOf rs) = getAllCourses rs
    getReqCourses (CreditReq _) = Set.empty

-- | Get all courses that can be taken *now* to progress on requirements
-- This explores the tree and returns candiates from active branches.
schedulableCourses :: [Requirement] -> Set.Set Course
schedulableCourses reqs = Set.unions (map getSchedulable reqs)
  where
    getSchedulable (CourseReq c) = Set.singleton c
    getSchedulable (OneOf rs) = 
        -- In a OneOf, any satisfying branch is a candidate. 
        -- We expose all options.
        schedulableCourses rs
    getSchedulable (AllOf rs) = 
        -- In an AllOf, we need to satisfy all.
        -- So all sub-requirements contribute candidates.
        schedulableCourses rs
    getSchedulable (CreditReq _) = Set.empty

-- | Simplify requirements given a set of completed courses.
-- Returns the *new* list of outstanding requirements.
-- If a requirement is satisfied, it is removed (or replaced with empty).
simplifyRequirements :: Set.Set CourseCode -> [Requirement] -> [Requirement]
simplifyRequirements completedCodes reqs = 
    concatMap (simplify completedCodes) reqs
  where
    simplify :: Set.Set CourseCode -> Requirement -> [Requirement]
    simplify done (CourseReq c) = 
        if code c `Set.member` done
        then [] -- Satisfied
        else [CourseReq c] 
    
    simplify done (OneOf rs) =
        let simplifiedSubs = map (\r -> simplify done r) rs
        in if any null simplifiedSubs 
           then [] -- At least one branch is fully satisfied (became empty list), so OneOf is satisfied
           else [OneOf (concat simplifiedSubs)] -- Keep trying simplified branches
           
    simplify done (AllOf rs) =
        let simplifiedSubs = concatMap (simplify done) rs
        in if null simplifiedSubs
           then [] -- All parts satisfied
           else [AllOf simplifiedSubs] -- Keep remaining parts
           
    simplify _ (CreditReq n) = [CreditReq n] -- Placeholder logic, not handling credit counting yet

-- ==========================================
-- CATALOG VALIDATION
-- ==========================================

-- | Check for courses that can never be scheduled
validateCatalog :: [Requirement] -> IO ()
validateCatalog reqs = do
    let catalog = getAllCourses reqs
        allCodes = Set.map code catalog
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
cleanCatalog :: [Requirement] -> [Requirement]
cleanCatalog reqs = 
    let allCourses = getAllCourses reqs
        allCodes = Set.map code allCourses
    in mapMaybe (cleanReq allCodes) reqs
  where
    mapMaybe f [] = []
    mapMaybe f (x:xs) = case f x of
        Nothing -> mapMaybe f xs
        Just y  -> y : mapMaybe f xs

    cleanReq :: Set.Set CourseCode -> Requirement -> Maybe Requirement
    cleanReq allCodes (CourseReq c) =
        if not (null (availability c)) && hasAllPrereqs allCodes (prereqs c)
        then Just (CourseReq c)
        else Nothing

    cleanReq allCodes (OneOf rs) =
        let validBranches = mapMaybe (cleanReq allCodes) rs
        in if null validBranches 
           then Nothing 
           else Just (OneOf validBranches)

    cleanReq allCodes (AllOf rs) =
        let validBranches = mapMaybe (cleanReq allCodes) rs
        in if length validBranches == length rs  -- ALL must be valid
           then Just (AllOf validBranches)
           else Nothing
           
    cleanReq _ (CreditReq n) = Just (CreditReq n)

    hasAllPrereqs allCodes tree = case tree of
        None -> True
        CourseCode prereqCode -> prereqCode `Set.member` allCodes
        And trees -> all (hasAllPrereqs allCodes) trees
        Or trees -> any (hasAllPrereqs allCodes) trees
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

-- | Calculate arbitrary depth estimate of requirement tree
-- We want to minimize this.
reqTreeDepth :: [Requirement] -> Int
reqTreeDepth reqs = maximum (0 : map depth reqs)
  where
    depth (CourseReq c) = 1 + prereqDepth (prereqs c) -- 1 for the course itself + its prereqs
    depth (OneOf rs) = minimum (map depth rs) -- We only need one branch, so take the easiest (min depth)
    depth (AllOf rs) = maximum (map depth rs) -- We need all, so we are blocked by the deepest
    depth (CreditReq _) = 0

    prereqDepth None = 0
    prereqDepth (CourseCode _) = 1
    prereqDepth (And ts) = maximum (0 : map prereqDepth ts)
    prereqDepth (Or ts) = minimum (map prereqDepth ts) -- assume we pick easiest path
    prereqDepth (CreditCount _) = 0

-- | Calculate the maximum prerequisite depth of remaining courses
maxPrereqDepth :: [Requirement] -> Set.Set CourseCode -> Int
maxPrereqDepth remaining _ = reqTreeDepth remaining

-- | Count courses that could be taken this semester but weren't
missedOpportunities :: PlannerState -> [Course] -> Int
missedOpportunities state taken =
    let eligible = Set.filter isAvailable (schedulableCourses (remainingReqs state))
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

-- | Estimate minimum number of course nodes remaining to be satisfied
countRemainingReqs :: [Requirement] -> Int
countRemainingReqs reqs = sum (map count reqs)
  where
    count (CourseReq _) = 1
    count (OneOf rs) 
        | null rs = 0 
        | otherwise = minimum (map count rs) -- Best case: pick shortest path
    count (AllOf rs) = sum (map count rs)
    count (CreditReq _) = 1

-- | Score a state (lower is better)
scoreState :: PlannerState -> Double
scoreState state
    | null (remainingReqs state) = 0.0  -- Completed - best score
    | otherwise =
        let semesterCount = fromIntegral $ Map.size (schedule state)
            -- Estimate remaining course count (accurate min-path)
            remainingCount = fromIntegral $ countRemainingReqs (remainingReqs state)
            depth = fromIntegral $ maxPrereqDepth (remainingReqs state) (completed state)
            variance = workloadVariance (schedule state)
            creditsPenalty = fromIntegral (totalCredits state) / 10.0 -- New: Penalize taking more credits than needed
        in semesterCount * 100           -- Minimize semesters (most important)
           + remainingCount * 10          -- Minimize remaining courses
           + depth * 5                    -- Minimize prerequisite bottlenecks
           + variance * 1                 -- Prefer balanced workload
           + creditsPenalty               -- Prefer efficient degree (fewest credits)

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
beamSearch :: Config -> Int -> PlannerState -> [PlannerState]
beamSearch config beamWidth initialState = go [initialState] (0 :: Int)
  where
    maxDepth = 50  -- Increased safety limit
    
    go beam depth
        | depth > maxDepth = []
        | all (null . remainingReqs) beam = beam  -- All complete
        | null beam = []
        | otherwise =
            let -- Generate successors for each state in beam
                successors = concatMap (expandState config) beam
                -- Score and sort all successors
                scored = sortBy (comparing scoreState) successors
                -- Keep only top beamWidth states
                nextBeam = take beamWidth scored
                -- Filter out completed states
                (completedStates, ongoing) = span (null . remainingReqs) nextBeam
                
                -- Debug output every 5 semesters
                _ = if depth `mod` 5 == 0 && depth > 0
                    then unsafePerformIO $ putStrLn $ 
                         "Depth " ++ show depth ++ ": " ++ 
                         show (length beam) ++ " states, " ++
                         "best has approx " ++ show (countRemainingReqs $ remainingReqs $ head beam) ++ " courses left"
                    else ()
            in if null completedStates
               then go ongoing (depth + 1)
               else completedStates ++ go ongoing (depth + 1)

-- | Expand a single state into all possible next states
expandState :: Config -> PlannerState -> [PlannerState]
expandState config state
    | null (remainingReqs state) = [state]
    | otherwise =
        let -- Get season limits
            currentS = season (currentSem state)
            minC = Map.findWithDefault 0 currentS (minCreditsPerSeason config)
            maxC = Map.findWithDefault 3 currentS (maxCreditsPerSeason config)
            bounds = (minC, maxC)

            -- Extract candidate basic courses from the requirement tree
            candidates = schedulableCourses (remainingReqs state)

            -- First check basic eligibility (prereqs + availability)
            -- Note: schedulableCourses gives us specific courses exposed by the tree.
            -- We still need to check their standard prereqs and availability.
            basicEligible = Set.filter isBasicAvailable candidates
            isBasicAvailable c = isEligible state (prereqs c) &&
                               currentS `elem` availability c
            
            -- Then filter for lab requirements
            eligible = Set.filter (hasLabAvailable basicEligible) basicEligible
            hasLabAvailable eligSet c = case requiresLab c of
                Nothing -> True
                Just labCode -> labCode `Set.member` completed state ||  -- Already completed
                              any (\ec -> code ec == labCode) (Set.toList eligSet)  -- Available now
            
            eligibleList = Set.toList eligible
            
            -- Debug logging
            _ = if null eligibleList && not (null (remainingReqs state))
                then unsafePerformIO $ do
                    putStrLn $ "  Warning: No eligible courses in " ++ 
                             show (season (currentSem state)) ++ " " ++ 
                             show (year (currentSem state))
                    putStrLn $ "    Remaining Requirements Top Level: " ++ show (length (remainingReqs state))
                    putStrLn $ "    Approx Remaining Courses: " ++ show (length (getAllCourses (remainingReqs state)))
                    putStrLn $ "    Schedulable Candidates: " ++ show (Set.size candidates)
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
            
            -- CRITICAL: Simplify the requirement tree based on new completions
            !newRemaining = simplifyRequirements newCompleted (remainingReqs st)

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
    , minCreditsPerSeason :: Map.Map Season Int
    , maxCreditsPerSeason :: Map.Map Season Int
    } deriving (Show)

defaultConfig :: FilePath -> Config
defaultConfig path = Config
    { catalogPath = path
    , beamWidth   = 5
    , startYear   = 2026
    , startSeason = Fall
    , minCreditsPerSeason = Map.fromList 
        [ (Fall, 3), (Winter, 3)
        , (Spring, 0), (Summer, 0)
        ]
    , maxCreditsPerSeason = Map.fromList 
        [ (Fall, 12), (Winter, 12)
        , (Spring, 3), (Summer, 3) 
        ]
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
    -- Global Limits
    parseOptions cfg ("--min-credits":m:xs) = 
        case reads m of
            [(n, "")] | n >= 0 -> 
                let newMap = Map.map (const n) (minCreditsPerSeason cfg)
                in parseOptions (cfg { minCreditsPerSeason = newMap }) xs
            _ -> Left $ "Invalid min-credits: " ++ m ++ " (must be >= 0)"
    parseOptions cfg ("--max-credits":m:xs) = 
        case reads m of
            [(n, "")] | n > 0 -> 
                let newMap = Map.map (const n) (maxCreditsPerSeason cfg)
                in parseOptions (cfg { maxCreditsPerSeason = newMap }) xs
            _ -> Left $ "Invalid max-credits: " ++ m ++ " (must be > 0)"
    
    -- Per-Season Limits (Spring)
    parseOptions cfg ("--min-credits-spring":m:xs) = updateSeasonMin cfg Spring m xs
    parseOptions cfg ("--max-credits-spring":m:xs) = updateSeasonMax cfg Spring m xs
    
    -- Per-Season Limits (Summer)
    parseOptions cfg ("--min-credits-summer":m:xs) = updateSeasonMin cfg Summer m xs
    parseOptions cfg ("--max-credits-summer":m:xs) = updateSeasonMax cfg Summer m xs
    
    -- Per-Season Limits (Fall)
    parseOptions cfg ("--min-credits-fall":m:xs) = updateSeasonMin cfg Fall m xs
    parseOptions cfg ("--max-credits-fall":m:xs) = updateSeasonMax cfg Fall m xs
    
    -- Per-Season Limits (Winter)
    parseOptions cfg ("--min-credits-winter":m:xs) = updateSeasonMin cfg Winter m xs
    parseOptions cfg ("--max-credits-winter":m:xs) = updateSeasonMax cfg Winter m xs

    parseOptions cfg ("--help":_) = Left helpText
    parseOptions cfg ("-h":_) = Left helpText
    parseOptions cfg (flag:_) | "--" `isPrefixOf` flag = 
        Left $ "Unknown option: " ++ flag ++ "\nUse --help for usage information"
    parseOptions _ (arg:_) = Left $ "Unexpected argument: " ++ arg

    updateSeasonMin cfg s m xs = case reads m of
        [(n, "")] | n >= 0 -> 
            let newMap = Map.insert s n (minCreditsPerSeason cfg)
            in parseOptions (cfg { minCreditsPerSeason = newMap }) xs
        _ -> Left $ "Invalid min-credits-" ++ show s ++ ": " ++ m

    updateSeasonMax cfg s m xs = case reads m of
        [(n, "")] | n > 0 -> 
            let newMap = Map.insert s n (maxCreditsPerSeason cfg)
            in parseOptions (cfg { maxCreditsPerSeason = newMap }) xs
        _ -> Left $ "Invalid max-credits-" ++ show s ++ ": " ++ m
    
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
        , "  --min-credits <n>           Set min credits for ALL seasons"
        , "  --max-credits <n>           Set max credits for ALL seasons"
        , "  --min-credits-spring <n>    Set min credits for Spring"
        , "  --max-credits-spring <n>    Set max credits for Spring (default: 3)"
        , "  --min-credits-summer <n>    Set min credits for Summer"
        , "  --max-credits-summer <n>    Set max credits for Summer (default: 3)"
        , "  ... (similar for Fall/Winter)"
        , "  -h, --help                  Show this help message"
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
        initialCourses = getAllCourses catalogRaw
        cleanCourses = getAllCourses catalog
    
    when (Set.size cleanCourses < Set.size initialCourses) $ do
        putStrLn $ "Removed " ++ show (Set.size initialCourses - Set.size cleanCourses) ++ 
                   " unsatisfiable courses."
        putStrLn $ "Scheduling approx " ++ show (Set.size cleanCourses) ++ " courses.\n"

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
    putStrLn $ "  Spring Max Credits: " ++ show (Map.findWithDefault 3 Spring (maxCreditsPerSeason config))
    putStrLn $ "  Summer Max Credits: " ++ show (Map.findWithDefault 3 Summer (maxCreditsPerSeason config))
    putStrLn $ "  Total requirements: " ++ show (length catalog) ++ "\n"
    
    let results = beamSearch config (beamWidth config) initialState
    
    case results of
        [] -> do
            putStrLn "No solutions found with current constraints."
            putStrLn "Trying with higher beam width (10)...\n"
            let relaxedResults = beamSearch config 10 initialState
            case relaxedResults of
                [] -> putStrLn "Error: No valid schedule could be generated."
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
