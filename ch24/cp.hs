{-# LANGUAGE OverloadedStrings #-}

import System.IO ()
import System.Environment ( getArgs )
import qualified Data.Text.IO as TI

main :: IO()
main = do
  args <- getArgs
  let source = args !! 0
  let dest = args !! 1
  contents <- TI.readFile source
  TI.writeFile dest contents