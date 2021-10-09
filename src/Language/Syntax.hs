{-# LANGUAGE DeriveDataTypeable #-}

module Language.Syntax where

import Data.Data

type Program = [Decl]

type Ident = String

data Decl = Decl Ident [Ident] Expr
  deriving (Show, Data)

data Expr = Num Int
          | Var Ident
          | Neg Expr
          | FunCall Ident [Expr]
          | Add {e1 :: Expr, e2 :: Expr}
          | Sub {e1 :: Expr, e2 :: Expr}
          | Mult {e1 :: Expr, e2 :: Expr}
          | Div {e1 :: Expr, e2 :: Expr}
          deriving (Show, Data)
