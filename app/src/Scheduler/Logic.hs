module Scheduler.Logic where

import qualified Data.Set as Set
import Scheduler.Types

-- | Flatten requirements to get all involved courses
getAllCourses :: [Requirement] -> Set.Set Course
getAllCourses reqs = Set.unions (map getReqCourses reqs)
  where
    getReqCourses (CourseReq c) = Set.singleton c
    getReqCourses (OneOf rs) = getAllCourses rs
    getReqCourses (AllOf rs) = getAllCourses rs
    getReqCourses (CreditReq _) = Set.empty

-- | Get all courses that can be taken *now* to progress on requirements
schedulableCourses :: [Requirement] -> Set.Set Course
schedulableCourses reqs = Set.unions (map getSchedulable reqs)
  where
    getSchedulable (CourseReq c) = Set.singleton c
    getSchedulable (OneOf rs) = schedulableCourses rs
    getSchedulable (AllOf rs) = schedulableCourses rs
    getSchedulable (CreditReq _) = Set.empty

-- | Simplify requirements given a set of completed courses.
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
           then [] -- At least one branch is fully satisfied
           else [OneOf (concat simplifiedSubs)]
           
    simplify done (AllOf rs) =
        let simplifiedSubs = concatMap (simplify done) rs
        in if null simplifiedSubs
           then [] -- All parts satisfied
           else [AllOf simplifiedSubs]
           
    simplify _ (CreditReq n) = [CreditReq n]

-- | Check if a course is eligible based on its prerequisite tree
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
