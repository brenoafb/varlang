{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Var
import Language.Syntax
import Language.Parser
import Text.Pretty.Simple (pPrint)
import qualified Data.Text as T

choice :: Dim -> [Expr] -> Expr
choice d es = VExpr $ Chc d (map Obj es)

vtwice = Dim "Impl" ["plus", "times"]
  $ Obj [ List
    [ Atom "define"
    , List
        [ Atom "twice"
        , Atom "x"
        ]
    , i
    ]
  ]
  where i = choice "Impl"
            [ List [ Atom "+", Atom "x", Atom "x" ]
            , List [ Atom "*", IntExpr 2, Atom "x" ]
            ]

main = do
  src <- getContents
  pPrint . parseStr $ T.pack src
