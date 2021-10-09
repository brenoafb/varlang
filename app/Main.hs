module Main where

import Data.Var
import Language.Syntax
import Language.Parser
import Text.Pretty.Simple (pPrint)

main = do
  src <- getContents
  pPrint $ parseStr src
