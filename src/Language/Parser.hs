{-# LANGUAGE OverloadedStrings #-}

module Language.Parser where

import Language.Syntax
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import qualified Data.Text as T

languageDef =
  emptyDef { Token.commentLine     = ";"
           , Token.identStart      = alphaNum <|> oneOf ":!#$%&*+./<=>?@\\^|-~_"
           , Token.identLetter     = alphaNum <|> oneOf ":!#$%&*+./<=>?@\\^|-~_"
           }

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer
parens = Token.parens lexer
int = fromIntegral <$> Token.natural lexer
double = Token.float lexer
whiteSpace = Token.whiteSpace lexer
comma = Token.comma lexer
stringLiteral = Token.stringLiteral lexer
reservedOp = Token.reservedOp lexer

parseStr :: T.Text -> Either ParseError Program
parseStr str = parse program "" (T.unpack str)

program :: Parser Program
program = whiteSpace >> many expr

expr :: Parser Expr
expr = try doubleExpr
  <|> try intExpr
  <|> quote
  <|> atom
  <|> stringExpr
  <|> list

doubleExpr :: Parser Expr
doubleExpr = DoubleExpr <$> double

intExpr :: Parser Expr
intExpr = IntExpr <$> int

atom :: Parser Expr
atom = Atom . T.pack <$> identifier

quote :: Parser Expr
quote = do
  _ <- char '\''
  Quote <$> expr

list :: Parser Expr
list = List <$> parens exprs
  where exprs = many expr

stringExpr :: Parser Expr
stringExpr = Str . T.pack <$> stringLiteral
