{-# LANGUAGE OverloadedStrings #-}
module Main where

import CC.Syntax
import Examples.Edit hiding (addAlt, extend)
import Data.Generics
import Language.Syntax
import Language.Parser
import Text.Pretty.Simple (pPrint)
import qualified Data.Text as T

type Ident = T.Text

choice :: Dim -> [Expr] -> Expr
choice d es = VExpr $ Chc d (map Obj es)

twice =
  List
    [ Atom "define"
    , List [ Atom "twice", Atom "x" ]
    , List [ Atom "+", Atom "x", Atom "x" ]
    ]

vtwice =
    Dim "Impl" ["plus", "times"]
  $ Dim "Par" ["x", "y"]
  $ Obj $ List
    [ Atom "define"
    , List [ Atom "twice", v ]
    , i
    ]
  where v = choice "Par" [Atom "x", Atom "y"]
        i = choice "Impl"
            [ List [ Atom "+", v , v ]
            , List [ Atom "*", IntExpr 2, v ]
            ]

addPar :: Expr -> Expr
addPar (Atom "x") = choice "Par" [Atom "x", Atom "y"]
addPar e = e

varyPar :: VExpr -> VExpr
varyPar = Dim "Par" ["x", "y"] . everywhere (mkT addPar)

addImpl :: Expr -> Expr
addImpl e@(List [ Atom "+", v, v' ])
   | v == v' = choice "Impl" [e, List [ Atom "*", IntExpr 2, v ]]
addImpl e = e

varyImpl :: VExpr -> VExpr
varyImpl = Dim "Impl" ["plus", "times"] . everywhere (mkT addImpl)

extend :: Data a => Dim -> Tag -> (V a -> V a) -> V a -> V a
extend d t f e = withFallback e $ do
  (c, Dim _ ts e) <- extract (dimDef d) e
  let e' = f `inRange` (chcFor d, dimDef d) $ e
  pure $ c <@ Dim d (ts ++ [t]) e'

swapAlt :: V a -> V a
swapAlt (Chc d [a1, a2]) = Chc d [a2, a1]

swapOptions :: Data a => Dim -> V a -> V a
swapOptions d e = withFallback e $ do
  (c, Dim _ ts e) <- extract (dimDef d) e
  let e' = swapAlt `inRange` (chcFor d, dimDef d) $ e
  pure $ c <@ Dim d (reverse ts) e'

addAlt :: V a -> V a -> V a
addAlt a (Chc d as) = Chc d (as ++ [a])

twiceZ :: VExpr
twiceZ = extend "Par" "z" (addAlt (Obj $ Atom "z")) vtwice

defn :: Pred Expr
defn (Obj (List [ Atom "define"
                , List [ Atom _, _ ]
                , _])) = True
defn _ = False

getArgs :: Expr -> Maybe [Expr]
getArgs (List [ Atom "define", List args, _ ]) = Just args
getArgs _ = Nothing

getBody :: Expr -> Maybe Expr
getBody (List [ Atom "define", _ , body ]) = Just body
getBody _ = Nothing

fun :: [Expr] -> Expr -> Expr
fun args body = List [ Atom "define", List args, body ]

renameRef :: Ident -> Ident -> Expr -> Expr
renameRef v0 v1 (Atom v)
  | v0 == v = Atom v1
renameRef _ _ e = e

renamePar :: VExpr -> Ident -> Ident -> VExpr
renamePar e v0 v1 = withFallback e $ do
  (c, Obj f) <- extract defn e
  args <- getArgs f
  body <- getBody f
  let args' = everywhere (mkT (renameRef v0 v1)) args
      body' = everywhere (mkT (renameRef v0 v1)) body
  return (c <@ Dim "Par" [T.unpack v0, T.unpack v1] (Obj $ fun args' body'))

main = do
  src <- getContents
  pPrint . parseStr $ T.pack src
