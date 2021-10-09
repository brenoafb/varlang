{-# LANGUAGE DeriveDataTypeable #-}

module Language.Syntax where

import Data.Data
import Data.Var

type Program = [Decl]

type Ident = String

data Decl = Decl Ident [Ident] Expr
  deriving (Show, Data)

type VExpr = V Expr

data Expr = Num Int
          | Var Ident
          | Neg Expr
          | FunCall Ident [Expr]
          | Add  { e1 :: Expr, e2 :: Expr }
          | Sub  { e1 :: Expr, e2 :: Expr }
          | Mult { e1 :: Expr, e2 :: Expr }
          | Div  { e1 :: Expr, e2 :: Expr }
          | VExpr VExpr
          deriving (Show, Data)
