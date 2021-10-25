{-# LANGUAGE OverloadedStrings #-}
module Analyzer where

import Debug.Trace (trace)
import Data.Generics
import Language.Syntax
import CC.Syntax
import Control.Applicative
import qualified Data.Text as T
import Data.List (nub)

isOperator :: T.Text -> Bool
isOperator = (`elem` ["+", "-", "*", "/"])

countOperations :: Expr -> Int
countOperations = everything (+) (0 `mkQ` f)
  where f (List ((Atom x):_))
          | isOperator x = 1
          | otherwise    = 0
        f _ = 0

vCountOperations :: V Expr -> V Int
vCountOperations ve = do
  e <- ve
  case e of
    (List [Atom x, e1, e2]) ->
      let childrenCount = (+) <$> vCountOperations (pure e1) <*> vCountOperations (pure e2)
       in if isOperator x
            then (+1) <$> childrenCount
            else childrenCount
    Quote e   -> vCountOperations $ pure e
    VExpr ve' -> vCountOperations ve'
    List es   -> sum <$> sequenceA vcs
      where vcs = map (vCountOperations . pure) es
    _ -> pure 0


countChars :: Expr -> Int
countChars = everything (+) (0 `mkQ` f)
  where f (Atom x) = T.length x
        f _ = 0

vCountChars :: V Expr -> V Int
vCountChars ve = do
  e <- ve
  case e of
    Atom t    -> pure $ T.length t
    Str  t    -> pure $ T.length t
    Quote q   -> vCountChars $ pure q
    VExpr ve' -> vCountChars ve'
    List es   -> sum <$> sequenceA vcs
      where vcs = map (vCountChars . pure) es
    _         -> pure 0

getAtoms :: Expr -> [T.Text]
getAtoms = nub . everything mappend ([] `mkQ` f)
  where f (Atom t) = [t]
        f _        = []

vGetAtoms :: V Expr -> V [T.Text]
vGetAtoms ve = do
  e <- ve
  case e of
    Atom t -> pure [t]
    Quote q -> vGetAtoms $ pure q
    VExpr ve' -> vGetAtoms ve'
    List es -> mconcat <$> sequenceA vcs
      where vcs = map (vGetAtoms . pure) es
    _ -> pure []
