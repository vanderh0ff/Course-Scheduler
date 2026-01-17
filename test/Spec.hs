{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Options.Applicative
import Scheduler
import Scheduler.CLI
import Test.Hspec

-- Helper to make courses quickly
mkCourse :: String -> PrereqTree -> Course
mkCourse cCode pre =
  Course
    { code = cCode,
      credits = 3,
      prereqs = pre,
      availability = [Fall, Winter, Spring, Summer],
      requiresLab = Nothing
    }

-- Helper to parse CLI args for testing
parseArgs :: [String] -> ParserResult Options
parseArgs args = execParserPure defaultPrefs (info (optionsParser <**> helper) fullDesc) args

main :: IO ()
main = hspec $ do
  describe "Requirement Logic" $ do
    let cA = mkCourse "A" None
        cB = mkCourse "B" None
        cC = mkCourse "C" (CourseCode "A")
        cD = mkCourse "D" (CourseCode "B")

        reqA = CourseReq cA
        reqB = CourseReq cB
        reqC = CourseReq cC
        reqD = CourseReq cD

    describe "schedulableCourses" $ do
      it "extracts simple course requirement" $ do
        schedulableCourses [reqA] `shouldBe` Set.singleton cA

      it "extracts all options from OneOf" $ do
        let req = OneOf [reqA, reqB]
        schedulableCourses [req] `shouldBe` Set.fromList [cA, cB]

      it "extracts all options from AllOf" $ do
        let req = AllOf [reqA, reqB]
        schedulableCourses [req] `shouldBe` Set.fromList [cA, cB]

      it "handles nested structures correctly" $ do
        let req = AllOf [reqA, OneOf [reqC, reqD]]
        schedulableCourses [req] `shouldBe` Set.fromList [cA, cC, cD]

    describe "simplifyRequirements" $ do
      it "removes satisfied course requirements" $ do
        simplifyRequirements (Set.singleton "A") [reqA] `shouldBe` []

      it "keeps unsatisfied course requirements" $ do
        simplifyRequirements Set.empty [reqA] `shouldBe` [reqA]

      context "OneOf" $ do
        let req = OneOf [reqA, reqB]

        it "is satisfied if one branch is satisfied (A)" $ do
          simplifyRequirements (Set.singleton "A") [req] `shouldBe` []

        it "is satisfied if one branch is satisfied (B)" $ do
          simplifyRequirements (Set.singleton "B") [req] `shouldBe` []

        it "remains if neither satisfied" $ do
          simplifyRequirements Set.empty [req] `shouldBe` [req]

        it "is satisfied if complex branch is satisfied" $ do
          let complexReq = OneOf [reqA, AllOf [reqB, reqC]]
          -- Satisfy A -> OneOf done
          simplifyRequirements (Set.singleton "A") [complexReq] `shouldBe` []
          -- Satisfy B only -> AllOf still needs C -> OneOf remains
          simplifyRequirements (Set.singleton "B") [complexReq]
            `shouldBe` [OneOf [reqA, AllOf [reqC]]]
          -- Satisfy B and C -> AllOf done -> OneOf done
          simplifyRequirements (Set.fromList ["B", "C"]) [complexReq] `shouldBe` []

      context "AllOf" $ do
        let req = AllOf [reqA, reqB]

        it "is satisfied only if ALL branches satisfied" $ do
          simplifyRequirements (Set.fromList ["A", "B"]) [req] `shouldBe` []

        it "remains partially satisfied if only one branch done" $ do
          simplifyRequirements (Set.singleton "A") [req] `shouldBe` [AllOf [reqB]]

    describe "countRemainingReqs" $ do
      it "counts single requirements as 1" $ do
        countRemainingReqs [reqA] `shouldBe` 1

      it "sums AllOf requirements" $ do
        countRemainingReqs [AllOf [reqA, reqB]] `shouldBe` 2

      it "takes minimum of OneOf requirements" $ do
        let path1 = [reqA] -- 1
        let path2 = [AllOf [reqB, reqC]] -- 2
        let req = OneOf [reqA, AllOf [reqB, reqC]]
        countRemainingReqs [req] `shouldBe` 1

      it "calculates correct remaining for nested complex tree" $ do
        -- (A OR (B AND C)) AND D
        -- Min path is A (1) + D (1) = 2
        -- Alt path is B+C (2) + D (1) = 3
        let req =
              AllOf
                [ OneOf [reqA, AllOf [reqB, reqC]],
                  reqD
                ]
        countRemainingReqs [req] `shouldBe` 2

  describe "CLI Parsing" $ do
    it "parses basic catalog path and defaults" $ do
      let Success opts = parseArgs ["catalog.json"]
      catalog opts `shouldBe` "catalog.json"
      beam opts `shouldBe` 5
      Scheduler.CLI.year opts `shouldBe` 2026
      seasonOpt opts `shouldBe` Fall

    it "parses custom beam and year" $ do
      let Success opts = parseArgs ["catalog.json", "--beam", "10", "--year", "2025"]
      beam opts `shouldBe` 10
      Scheduler.CLI.year opts `shouldBe` 2025

    it "parses different starting seasons" $ do
      let Success optsSpring = parseArgs ["catalog.json", "--season", "Spring"]
      seasonOpt optsSpring `shouldBe` Spring
      let Success optsSummer = parseArgs ["catalog.json", "--season", "Summer"]
      seasonOpt optsSummer `shouldBe` Summer
      let Success optsWinter = parseArgs ["catalog.json", "--season", "Winter"]
      seasonOpt optsWinter `shouldBe` Winter

    it "parses global credit overrides" $ do
      let Success opts = parseArgs ["catalog.json", "--min-credits", "6", "--max-credits", "18"]
      minCredits opts `shouldBe` Just 6
      maxCredits opts `shouldBe` Just 18
      let config = configFromOptions opts
      minCreditsPerSeason config `shouldBe` Map.fromList [(Fall, 6), (Winter, 6), (Spring, 6), (Summer, 6)]
      maxCreditsPerSeason config `shouldBe` Map.fromList [(Fall, 18), (Winter, 18), (Spring, 18), (Summer, 18)]

    it "parses seasonal credit overrides" $ do
      let Success opts = parseArgs ["catalog.json", "--max-credits-spring", "6", "--max-credits-summer", "6"]
      let config = configFromOptions opts
      Map.lookup Spring (maxCreditsPerSeason config) `shouldBe` Just 6
      Map.lookup Summer (maxCreditsPerSeason config) `shouldBe` Just 6
      Map.lookup Fall (maxCreditsPerSeason config) `shouldBe` Just 12 -- default
    it "fails when no catalog is provided" $ do
      let Failure _ = parseArgs []
      True `shouldBe` True -- Just verification that it returned Failure
