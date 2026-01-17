{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import System.Environment (getArgs)
import Data.List (isPrefixOf, sortOn)
import Control.Monad (when)

import Scheduler

-- ==========================================
-- CLI ARGUMENT PARSING
-- ==========================================

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
            Just ssn -> parseOptions (cfg { startSeason = ssn }) xs
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

    parseOptions _ ("--help":_) = Left helpText
    parseOptions _ ("-h":_) = Left helpText
    parseOptions _ (flag:_) | "--" `isPrefixOf` flag = 
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
