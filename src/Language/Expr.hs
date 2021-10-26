{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Expr where

import Data.Data
import CC.Syntax

import qualified Data.Map as M
import qualified Data.Text as T

type Program = [Expr]

type Ident = T.Text
type Frame = M.Map T.Text Expr
type Env = [Frame]
type Error = T.Text

-- variability-aware expression
type VExpr = V Expr

data Expr = Atom       Ident
          | Str        T.Text
          | IntExpr    Int
          | DoubleExpr Double
          | Quote      Expr
          | List       [Expr]
          | VExpr      VExpr
          deriving Data

true :: Expr
true = Atom "#t"

nil :: Expr
nil  = List []

instance Show Expr where
  show (Atom t)       = T.unpack $ "Atom " <> t
  show (Str t)        = T.unpack $ "Str " <> "\"" <> t <> "\""
  show (IntExpr x)    = "IntExpr " ++ show x
  show (DoubleExpr x) = "DoubleExpr " ++ show x
  show (Quote x)      = "Quote " ++ show x
  show (List xs)      = "List " ++ show xs
  show (VExpr ve)     = "VExpr " ++ show ve

display :: Expr -> T.Text
display (Atom t)       = t
display (Str t)        = "\"" <> t <> "\""
display (IntExpr x)    = T.pack $ show x
display (DoubleExpr x) = T.pack $ show x
display (Quote t)      = "'" <> display t
display (List xs)      = "(" <> T.unwords (map display xs) <> ")"
display (VExpr ve)     = "[| " <> T.pack (show ve) <> " |]"

instance Eq Expr where
  (Atom x)       == (Atom y)       = x == y
  (Str  x)       == (Str  y)       = x == y
  (IntExpr x)    == (IntExpr y)    = x == y
  (DoubleExpr x) == (DoubleExpr y) = x == y
  (Quote x)      == (Quote y)      = x == y
  (List xs)      == (List ys)      = xs == ys
  (VExpr v1)     == (VExpr v2)     = v1 == v2
  _              == _              = False
