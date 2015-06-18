import Rainbow
import System.Directory
import Data.Foldable (forM_)
import qualified Rainbow.Types as Y
import qualified System.FilePath.Find as Find

main :: IO ()
main = do
    dir <- getCurrentDirectory
    files <- Find.find Find.always (Find.fileName Find.~~? "lint-results.xml") dir
    forM_ files (\x -> putChunkLn $ chunk x & fore blue)
