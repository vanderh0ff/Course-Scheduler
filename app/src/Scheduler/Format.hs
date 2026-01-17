module Scheduler.Format where

import qualified Data.Map.Strict as Map
import Data.List (intercalate, sortOn)
import Scheduler.Types
import Scheduler.Solver (scoreState)

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
