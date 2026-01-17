{-# LANGUAGE OverloadedStrings #-}

module Scheduler.CLI
  ( Options (..),
    optionsParser,
    configFromOptions,
  )
where

import qualified Data.Map.Strict as Map
import Options.Applicative
import Scheduler.Types

data Options = Options
  { catalog :: FilePath,
    beam :: Int,
    year :: Int,
    seasonOpt :: Season,
    minCredits :: Maybe Int,
    maxCredits :: Maybe Int,
    minSpring :: Maybe Int,
    maxSpring :: Maybe Int,
    minSummer :: Maybe Int,
    maxSummer :: Maybe Int,
    minFall :: Maybe Int,
    maxFall :: Maybe Int,
    minWinter :: Maybe Int,
    maxWinter :: Maybe Int
  }

optionsParser :: Parser Options
optionsParser =
  Options
    <$> argument str (metavar "CATALOG" <> help "Path to course catalog JSON file")
    <*> option auto (long "beam" <> value 5 <> showDefault <> help "Beam search width")
    <*> option auto (long "year" <> value 2026 <> showDefault <> help "Starting year")
    <*> option auto (long "season" <> value Fall <> showDefault <> help "Starting season (Fall, Winter, Spring, Summer)")
    -- Global Limits
    <*> optional (option auto (long "min-credits" <> help "Set min credits for ALL seasons"))
    <*> optional (option auto (long "max-credits" <> help "Set max credits for ALL seasons"))
    -- Per-Season Limits
    <*> optional (option auto (long "min-credits-spring" <> help "Set min credits for Spring"))
    <*> optional (option auto (long "max-credits-spring" <> help "Set max credits for Spring (default: 3)"))
    <*> optional (option auto (long "min-credits-summer" <> help "Set min credits for Summer"))
    <*> optional (option auto (long "max-credits-summer" <> help "Set max credits for Summer (default: 3)"))
    <*> optional (option auto (long "min-credits-fall" <> help "Set min credits for Fall"))
    <*> optional (option auto (long "max-credits-fall" <> help "Set max credits for Fall (default: 12)"))
    <*> optional (option auto (long "min-credits-winter" <> help "Set min credits for Winter"))
    <*> optional (option auto (long "max-credits-winter" <> help "Set max credits for Winter (default: 12)"))

configFromOptions :: Options -> Config
configFromOptions opts =
  let baseMinMap = Map.fromList [(Fall, 3), (Winter, 3), (Spring, 0), (Summer, 0)]
      baseMaxMap = Map.fromList [(Fall, 12), (Winter, 12), (Spring, 3), (Summer, 3)]

      -- Apply global overrides
      minMap1 = maybe baseMinMap (\n -> Map.map (const n) baseMinMap) (minCredits opts)
      maxMap1 = maybe baseMaxMap (\n -> Map.map (const n) baseMaxMap) (maxCredits opts)

      -- Apply seasonal overrides
      minMapFinal =
        applyMaybe minSpring Spring $
          applyMaybe minSummer Summer $
            applyMaybe minFall Fall $
              applyMaybe minWinter Winter minMap1

      maxMapFinal =
        applyMaybe maxSpring Spring $
          applyMaybe maxSummer Summer $
            applyMaybe maxFall Fall $
              applyMaybe maxWinter Winter maxMap1
   in Config
        { catalogPath = catalog opts,
          beamWidth = beam opts,
          startYear = Scheduler.CLI.year opts,
          startSeason = seasonOpt opts,
          minCreditsPerSeason = minMapFinal,
          maxCreditsPerSeason = maxMapFinal
        }
  where
    applyMaybe f s m = maybe m (\n -> Map.insert s n m) (f opts)
