{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE OverloadedStrings         #-}
module Main where

import           BasicPrelude                hiding (fromString)

import           Test.Hspec
import           Test.QuickCheck             hiding (verbose)
import           Test.QuickCheck.Monadic
import           Test.QuickCheck.Property    hiding (verbose)

import           AndroidLintSummary
import           AndroidLintSummary.CLI
import           Control.Lens                ((^.))
import           Data.Stringable             (Stringable (toString, fromString))
import           Data.Version                (makeVersion)
import           System.Directory            (getCurrentDirectory)
import           Text.XML.HXT.Core

import           Control.Exception           (evaluate)
import           Options.Applicative.Builder (prefs)
import           Options.Applicative.Extra   (execParserMaybe, execParserPure,
                                              getParseResult)


instance Arbitrary LintSeverity where
    arbitrary = elements allSeverities

instance Arbitrary LintFormatter where
    arbitrary = elements allFormatters

allSeverities :: [LintSeverity]
allSeverities = [minBound ..]

allFormatters :: [LintFormatter]
allFormatters = [minBound ..]

openFixture :: forall s b. FilePath -> IO (IOStateArrow s b XmlTree)
openFixture path = do
    dir <- getCurrentDirectory
    openXMLFile $ dir </> "Tests" </> "fixtures" </> path

main :: IO ()
main = hspec $ do
    describe "LintSeverity" $ do
        it "fromString . toString = id" . property $ do
          \x -> (fromString . toString) x == (x :: LintSeverity)

    describe "XML Parser" $ do
        it "reads an empty file" $ do
            file <- liftIO . openFixture $ "0" </> "app" </> "build" </> "outputs" </> "lint-results.xml"
            issues <- liftIO $ readLintIssues file

            issues `shouldBe` mempty

        it "reads a non-empty file" $ do
            file <- liftIO . openFixture $ "1" </> "lint-results.xml"
            issues <- liftIO $ readLintIssues file

            length issues `shouldBe` 2

            let iss0 = head issues
            iss0 ^. severity `shouldBe` WarningSeverity
            iss0 ^. priority `shouldBe` 6

            let loc0 = iss0 ^. location
            loc0 ^. filename `shouldBe` "/home/pascal/Projects/java/Android-DirectoryChooser/library/build.gradle"
            loc0 ^. line `shouldBe` Just 25
            loc0 ^. column `shouldBe` Just 5

        it "reads a file with Information severity" $ do
            file <- liftIO . openFixture $ "3" </> "lint-results.xml"
            issues <- liftIO $ readLintIssues file

            length issues `shouldBe` 88

            let infos = filter (\i -> i ^. severity == InformationalSeverity) issues
            length infos `shouldBe` 3

    describe "CLI Argument parser" $ do
        let version = makeVersion [0, 3, 1]
        let parser = lintSummaryParser version
        let defaultPrefs = prefs mempty
        let parse args = getParseResult $ execParserPure defaultPrefs parser args

        it "doesn't run the app when querying the version" $
            parse ["-V"] `shouldBe` Nothing

        it "verbose mode" $ do
            let res = parse ["-v"]
            let (Just opts) = res
            opts ^. verbose `shouldBe` Verbose

        it "normal mode" $ do
            let res = parse [""]
            let (Just opts) = res
            opts ^. verbose `shouldBe` Normal
            opts ^. formatter `shouldBe` SimpleLintFormatter

        it "glob" $ do
            let res = parse ["-g", "**/my/glob.*"]
            let (Just opts) = res
            opts ^. glob `shouldBe` "**/my/glob.*"

        it "null formatter" $ do
            let res = parse ["-f", "null"]
            let (Just opts) = res
            opts ^. formatter `shouldBe` NullLintFormatter

        it "simple formatter" $ do
            let res = parse ["-f", "simple"]
            let (Just opts) = res
            opts ^. formatter `shouldBe` SimpleLintFormatter

        it "invalid formatter" $ do
            let res = parse ["-f", "invalid"]
            let (Just opts) = res
            evaluate (opts ^. formatter) `shouldThrow` errorCall "Invalid LintFormatter specification"
