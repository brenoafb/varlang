module Main where

import Data.Var
import Language.Syntax
import Language.Parser
import Text.Pretty.Simple (pPrint)

program =
  [ Decl "twice" [ "x" ]
      ( Mult
          { e1 = Num 2
          , e2 = Var "x"
          }
      )
  , Decl "main" []
      ( FunCall "twice"
          [ Num 2 ]
      )
  ]

twice =
  Decl "twice" [ "x" ]
    ( Mult
        { e1 = Num 2
        , e2 = Var "x"
        }
    )

twice' =
  Decl "twice" [ "x" ]
    ( Add
        { e1 = Var "x"
        , e2 = Var "x"
        }
    )

vtwice = Dim "Impl" ["plus", "times"]
  $ Obj (Decl "twice" [ "x" ] i)
  where i = choice "Impl" [
              ( Add  { e1 = Var "x", e2 = Var "x" }),
              ( Mult { e1 = Num 2, e2 = Var "x" })
            ]

choice :: Dim -> [Expr] -> Expr
choice d es = VExpr $ Chc d (map Obj es)

main = do
  src <- getContents
  pPrint $ parseStr src
