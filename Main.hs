import Rainbow
import System.Directory
import Text.HandsomeSoup
import Text.XML.HXT.Core

import Data.Foldable (forM_)

import qualified System.FilePath.Find as Find

formatLintResults :: FilePath -> IO ()
formatLintResults filename = do
    contents <- readFile filename
    let doc = readString [withWarnings yes] contents
    links <- runX $ doc >>> css "issues issue" ! "summary"
    forM_ links putStrLn

main :: IO ()
main = do
    dir <- getCurrentDirectory
    files <- Find.find Find.always (Find.fileName Find.~~? "lint-results.xml") dir
    forM_ files formatLintResults
    -- forM_ files (\x -> putChunkLn $ chunk x & fore blue)
