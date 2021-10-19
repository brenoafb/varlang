{-# LANGUAGE OverloadedStrings #-}
module Analyzer where

import Data.Generics
import Language.Syntax
import CC.Syntax
import Control.Applicative
import qualified Data.Text as T

countOperations :: Expr -> Int
countOperations = everything (+) (0 `mkQ` f)
  where f (List ((Atom x):_))
          | isOperator x = 1
          | otherwise    = 0
        f _ = 0

vCountOperations :: V Expr -> V Int
vCountOperations = everything (liftA2 (+)) (pure 0 `mkQ` f)
  where f :: V Expr -> V Int
        f ve = do
          e <- ve
          case e of
            (List ((Atom x):xs))
              | isOperator x -> pure 1
              | otherwise    -> pure 0
            _ -> pure 0

vCountOperations' :: V Expr -> V Int
vCountOperations' = fmap countOperations

isOperator :: T.Text -> Bool
isOperator = (`elem` ["+", "-", "*", "/"])
