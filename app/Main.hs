{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad (unless, when)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Options.Applicative
import Scheduler
import Scheduler.CLI

-- ==========================================
-- MAIN
-- ==========================================

main :: IO ()
main = do
  opts <-
    execParser $
      info
        (optionsParser <**> helper)
        (fullDesc <> progDesc "Course Scheduler - Beam Search Planner" <> header "planner - a college course scheduler")
  runPlanner (configFromOptions opts)

runPlanner :: Config -> IO ()
runPlanner config = do
  putStrLn "Loading Catalog...\n"
  res <- loadCatalog (catalogPath config)
  case res of
    Left err -> putStrLn $ "Error: " ++ err
    Right catalogRaw -> do
      -- Validate catalog
      let issues = validateCatalog catalogRaw
      unless (null issues) $ do
        putStrLn "WARNING: Found issues in catalog:"
        mapM_ putStrLn issues
        putStrLn ""

      let catalog = cleanCatalog catalogRaw
          initialCourses = getAllCourses catalogRaw
          cleanCourses = getAllCourses catalog

      when (Set.size cleanCourses < Set.size initialCourses) $ do
        putStrLn $
          "Removed "
            ++ show (Set.size initialCourses - Set.size cleanCourses)
            ++ " unsatisfiable courses."
        putStrLn $ "Scheduling approx " ++ show (Set.size cleanCourses) ++ " courses.\n"

      let initialState =
            PlannerState
              { remainingReqs = catalog,
                completed = Set.empty,
                currentSem = Semester (startYear config) (startSeason config),
                schedule = Map.empty,
                totalCredits = 0
              }

      putStrLn "Running Beam Search...\n"
      putStrLn "Configuration:"
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
      mapM_
        ( \(idx :: Int, sol) -> do
            putStrLn $ "\n========== Solution #" ++ show idx ++ " =========="
            putStrLn $ formatSolution sol
            putStrLn $ replicate 60 '='
            putStrLn $ formatSchedule (schedule sol)
            putStrLn ""
        )
        (zip [1 ..] topSolutions)
