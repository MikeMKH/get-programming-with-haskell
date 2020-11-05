{-# LANGUAGE OverloadedStrings #-}

import System.IO ()
import System.Environment ( getArgs )
import qualified Data.Text as T
import qualified Data.Text.IO as TI

main :: IO()
main = do
  args <- getArgs
  let source = args !! 0
  let dest = mconcat [source, ".capitalize"]
  content <- TI.readFile source
  TI.writeFile dest $ T.toUpper content