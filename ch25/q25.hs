{-# LANGUAGE OverloadedStrings #-}

import System.Environment ( getArgs )
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T
import qualified Data.Text.Encoding as E

-- 道可道，非常道。名可名，非常名。无名天地之始；有名万物之母。故常无欲，以观其妙；常有欲，以观其徼。此两者，同出而异名，同谓之玄。玄之又玄，衆妙之门。
-- 老子, 1
main :: IO()
main = do
  args <- getArgs
  let fileName = head args
  contents <- BC.readFile fileName
  let numberBytes = BC.length contents
  let numberCharacters = (T.length . E.decodeUtf8) contents
  print $ mconcat ["bytes: ", show numberBytes]
  print $ mconcat ["characters: ", show numberCharacters]
  print $ mconcat ["difference: ", show (numberBytes - numberCharacters)]