module Scheduler.Catalog where

import qualified Data.Set as Set
import qualified Data.ByteString.Lazy as B
import Data.Aeson (decode)
import Data.Maybe (mapMaybe)
import Scheduler.Types
import Scheduler.Logic

loadCatalog :: FilePath -> IO (Either String [Requirement])
loadCatalog path = do
    content <- B.readFile path
    case decode content of
        Just reqs -> return (Right reqs)
        Nothing   -> return (Left "Failed to parse course catalog. Check JSON format.")

-- | Check for courses that can never be scheduled
validateCatalog :: [Requirement] -> [String]
validateCatalog reqs = 
    let catalog = getAllCourses reqs
        allCodes = Set.map code catalog
    in concatMap (checkCourse allCodes) (Set.toList catalog)
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
        in if length validBranches == length rs
           then Just (AllOf validBranches)
           else Nothing
           
    cleanReq _ (CreditReq n) = Just (CreditReq n)

    hasAllPrereqs allCodes tree = case tree of
        None -> True
        CourseCode prereqCode -> prereqCode `Set.member` allCodes
        And trees -> all (hasAllPrereqs allCodes) trees
        Or trees -> any (hasAllPrereqs allCodes) trees
        CreditCount _ -> True
