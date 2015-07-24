{-# LANGUAGE OverloadedStrings, NoImplicitPrelude, ExistentialQuantification #-}
module Main where

import BasicPrelude hiding (fromString)

import Test.Hspec
import Test.QuickCheck hiding (verbose)
import Test.QuickCheck.Property hiding (verbose)
import Test.QuickCheck.Monadic

import AndroidLintSummary
import AndroidLintSummary.CLI
import Data.Stringable (Stringable(toString, fromString))
import System.Directory (getCurrentDirectory)
import Text.XML.HXT.Core
import Data.Version (makeVersion)

import Options.Applicative.Builder (prefs)
import Options.Applicative.Extra (execParserPure, getParseResult, execParserMaybe)


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

    describe "LintFormatter" $ do
        it "fromString . toString = id" . property $ do
          \x -> (fromString . toString) x == (x :: LintFormatter)

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
            severity iss0 `shouldBe` WarningSeverity
            priority iss0 `shouldBe` 6

            let loc0 = location iss0
            filename loc0 `shouldBe` "/home/pascal/Projects/java/Android-DirectoryChooser/library/build.gradle"
            line loc0 `shouldBe` Just 25
            column loc0 `shouldBe` Just 5

        it "reads a file with Information severity" $ do
            file <- liftIO . openFixture $ "3" </> "lint-results.xml"
            issues <- liftIO $ readLintIssues file

            length issues `shouldBe` 88

            let infos = filter (\i -> severity i == InformationalSeverity) issues
            length infos `shouldBe` 3

    describe "CLI Argument parser" $ do
        let version = makeVersion [0, 3, 1]
        let parser = lintSummaryParser version

        it "doesn't run the app when querying the version" $ do
            execParserMaybe parser ["-V"] `shouldBe` Nothing

        it "verbose mode" $ do
            let res = execParserMaybe parser ["-v"]
            let (Just opts) = res
            verbose opts `shouldBe` Verbose

        it "normal mode" $ do
            let res = execParserMaybe parser [""]
            let (Just opts) = res
            verbose opts `shouldBe` Normal
