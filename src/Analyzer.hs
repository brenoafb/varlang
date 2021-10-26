{-# LANGUAGE OverloadedStrings #-}
module Analyzer where

import Debug.Trace (trace)
import Data.Generics
import Language.Expr
import CC.Syntax
import Control.Applicative
import Data.List (nub)

import qualified Language.PExpr as P
import qualified Data.Text as T

-- count arithmetic operations in an expression
countOperations :: P.Expr -> Int
countOperations = everything (+) (0 `mkQ` f)
  where f (P.List ((P.Atom x):_))
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

isOperator :: T.Text -> Bool
isOperator = (`elem` ["+", "-", "*", "/"])

-- count characters in strings and atoms
countChars :: P.Expr -> Int
countChars = everything (+) (0 `mkQ` f)
  where f (P.Atom t) = T.length t
        f (P.Str  t) = T.length t
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

-- get a list of unique atoms in the expression
getAtoms :: P.Expr -> [Ident]
getAtoms = nub . everything mappend ([] `mkQ` f)
  where f (P.Atom t) = [t]
        f _        = []

vGetAtoms :: V Expr -> V [Ident]
vGetAtoms ve = nub <$> result
  where result = do
          e <- ve
          case e of
            Atom t -> pure [t]
            Quote q -> vGetAtoms $ pure q
            VExpr ve' -> vGetAtoms ve'
            List es -> mconcat <$> sequenceA vcs
              where vcs = map (vGetAtoms . pure) es
            _ -> pure []
