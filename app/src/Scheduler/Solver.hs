{-# LANGUAGE BangPatterns #-}
module Scheduler.Solver where

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.List (sortOn, sortBy, subsequences)
import Data.Ord (comparing, Down(..))
import Control.Monad (guard)
import Scheduler.Types
import Scheduler.Logic

-- ==========================================
-- HEURISTIC SCORING FUNCTIONS
-- ==========================================

reqTreeDepth :: [Requirement] -> Int
reqTreeDepth reqs = maximum (0 : map depth reqs)
  where
    depth (CourseReq c) = 1 + prereqDepth (prereqs c)
    depth (OneOf rs) = minimum (map depth rs)
    depth (AllOf rs) = maximum (map depth rs)
    depth (CreditReq _) = 0

    prereqDepth None = 0
    prereqDepth (CourseCode _) = 1
    prereqDepth (And ts) = maximum (0 : map prereqDepth ts)
    prereqDepth (Or ts) = minimum (map prereqDepth ts)
    prereqDepth (CreditCount _) = 0

maxPrereqDepth :: [Requirement] -> Set.Set CourseCode -> Int
maxPrereqDepth remaining _ = reqTreeDepth remaining

workloadVariance :: Map.Map Semester [Course] -> Double
workloadVariance sched =
    let creditCounts = map (sum . map credits) (Map.elems sched)
        avg = fromIntegral (sum creditCounts) / fromIntegral (max 1 (length creditCounts))
        variance = sum [((fromIntegral c) - avg) ** 2 | c <- creditCounts] / fromIntegral (max 1 (length creditCounts))
    in variance

countRemainingReqs :: [Requirement] -> Int
countRemainingReqs reqs = sum (map count reqs)
  where
    count (CourseReq _) = 1
    count (OneOf rs) 
        | null rs = 0 
        | otherwise = minimum (map count rs)
    count (AllOf rs) = sum (map count rs)
    count (CreditReq _) = 1

scoreState :: PlannerState -> Double
scoreState state
    | null (remainingReqs state) = 0.0
    | otherwise =
        let semesterCount = fromIntegral $ Map.size (schedule state)
            remainingCount = fromIntegral $ countRemainingReqs (remainingReqs state)
            depth = fromIntegral $ maxPrereqDepth (remainingReqs state) (completed state)
            variance = workloadVariance (schedule state)
            creditsPenalty = fromIntegral (totalCredits state) / 10.0
        in semesterCount * 100
           + remainingCount * 10
           + depth * 5
           + variance * 1
           + creditsPenalty

-- ==========================================
-- COURSE COMBINATION GENERATION
-- ==========================================

pickCombinations :: [Course] -> (Int, Int) -> Int -> Set.Set CourseCode -> [[Course]]
pickCombinations eligible (minC, maxC) maxCombos alreadyCompleted = 
    let allSubsets = subsequences eligible
        valid = do
            subset <- allSubsets
            let totalC = sum (map credits subset)
            guard $ totalC >= minC && totalC <= maxC
            guard $ all (hasRequiredLab subset) subset
            return subset
        sorted = sortBy (comparing (Down . sum . map credits)) valid
        result = take maxCombos sorted
    in result
  where
    hasRequiredLab selection c = case requiresLab c of
        Nothing      -> True
        Just labCode -> labCode `Set.member` alreadyCompleted ||
                       any (\subC -> code subC == labCode) selection

-- ==========================================
-- BEAM SEARCH IMPLEMENTATION
-- ==========================================

nextSemester :: Semester -> Semester
nextSemester (Semester y Fall)   = Semester y Winter
nextSemester (Semester y Winter) = Semester y Spring
nextSemester (Semester y Spring) = Semester y Summer
nextSemester (Semester y Summer) = Semester (y + 1) Fall

beamSearch :: Config -> Int -> PlannerState -> [PlannerState]
beamSearch config userBeamWidth initialState = go [initialState] (0 :: Int)
  where
    maxDepth = 50
    
    go beam depth
        | depth > maxDepth = []
        | all (null . remainingReqs) beam = beam
        | null beam = []
        | otherwise =
            let successors = concatMap (expandState config) beam
                scored = sortBy (comparing scoreState) successors
                nextBeam = take userBeamWidth scored
                (completedStates, ongoing) = span (null . remainingReqs) nextBeam
            in if null completedStates
               then go ongoing (depth + 1)
               else completedStates ++ go ongoing (depth + 1)

expandState :: Config -> PlannerState -> [PlannerState]
expandState config state
    | null (remainingReqs state) = [state]
    | otherwise =
        let currentS = season (currentSem state)
            minC = Map.findWithDefault 0 currentS (minCreditsPerSeason config)
            maxC = Map.findWithDefault 3 currentS (maxCreditsPerSeason config)
            bounds = (minC, maxC)

            candidates = schedulableCourses (remainingReqs state)

            basicEligible = Set.filter isBasicAvailable candidates
            isBasicAvailable c = isEligible state (prereqs c) &&
                               currentS `elem` availability c
            
            eligible = Set.filter (hasLabAvailable basicEligible) basicEligible
            hasLabAvailable eligSet c = case requiresLab c of
                Nothing -> True
                Just labCode -> labCode `Set.member` completed state ||
                              any (\ec -> code ec == labCode) (Set.toList eligSet)
            
            eligibleList = Set.toList eligible
            
            maxCombosPerState = 20
            combos = if null eligibleList
                     then [[]]
                     else pickCombinations eligibleList bounds maxCombosPerState (completed state)
            
        in map (applyCombo state) combos
  where
    applyCombo st courseCombo =
        let !newCompleted = completed st `Set.union` 
                           Set.fromList (map code courseCombo)
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
