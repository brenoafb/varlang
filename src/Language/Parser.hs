module Language.Parser
  ( parseStr
  ) where

import Language.Syntax

import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

languageDef =
  emptyDef { Token.commentStart    = "/*"
           , Token.commentEnd      = "*/"
           , Token.commentLine     = "//"
           , Token.identStart      = letter
           , Token.identLetter     = alphaNum
           , Token.reservedNames   = []
           , Token.reservedOpNames = [ "+", "-", "*", "/"
                                     , "="
                                     ]
           }

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer
reserved = Token.reserved lexer
reservedOp = Token.reservedOp lexer
parens = Token.parens lexer
int = fromIntegral <$> Token.integer lexer
semi = Token.semi lexer
whiteSpace = Token.whiteSpace lexer
braces = Token.braces lexer
comma = Token.comma lexer

program :: Parser Program
program = whiteSpace >> many decl

decl :: Parser Decl
decl = do
  name <- identifier
  args <- parens (identifier `sepBy` comma)
  reservedOp "="
  body <- expr
  return $ Decl name args body

expr :: Parser Expr
expr = try funCall <|> buildExpressionParser operators term

funCall :: Parser Expr
funCall = do
  funName <- identifier
  args <- parens (sepBy expr comma)
  return $ FunCall funName args

operators = [ [Prefix (reservedOp "-" >> return Neg)]
            , [Infix  (reservedOp "*" >> return Mult) AssocLeft,
               Infix  (reservedOp "/" >> return Div) AssocLeft]
            , [Infix  (reservedOp "+" >> return Add) AssocLeft,
               Infix  (reservedOp "-" >> return Sub) AssocLeft]
            ]

term = parens expr
     <|>  Var <$> identifier
     <|> Num <$> int

parseStr :: String -> Program
parseStr str = case parse program "" str of
                 Left e -> error $ show e
                 Right r -> r
