import Rainbow
import qualified Rainbow.Types as Y

import Control.Lens

main :: IO ()
main = do
    putChunkLn $ chunk "hello" & fore blue
