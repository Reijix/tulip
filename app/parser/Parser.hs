module Parser (Parser.parse) where

import Data.Functor
import Debug.Trace (trace)
import Lexer
import Syntax
import Text.Parsec hiding (char)
import Text.Parsec.String
import qualified Text.Parsec.Token as Tok
import Prelude hiding (EQ, GT, LT)

-- helpers
identifier :: Parser Identifier
identifier = identifierStr

op :: Parser Identifier
op = opStr

-- literals
literal :: Parser Literal
literal = try intLit <|> try realLit <|> try charLit

intLit :: Parser Literal
intLit = IntLit <$> int

realLit :: Parser Literal
realLit = RealLit <$> float

charLit :: Parser Literal
charLit = CharLit <$> char

-- program
program :: Parser Program
program = Program <$> endBy declaration semi

-- declarations
declaration :: Parser Declaration
declaration = 
  try dataDeclaration 
    <|> try functionDeclaration 
    <?> "declaration"

functionDeclaration :: Parser Declaration
functionDeclaration = 
  FunctionDeclaration 
    <$> identifier
    <*> many identifier
    <*> (reserved "=" *> expression)

dataDeclaration :: Parser Declaration
dataDeclaration = 
  DataDeclaration
    <$> identifier
    <*> (reserved "=" *> reserved "data" *> int)

-- expressions
expression :: Parser Expression
expression =
  try eCase
  <|> try eApp
  <?> "expression"

eConstant :: Parser Expression
eConstant =
  EConstant
    <$> literal

eCase :: Parser Expression
eCase =
  ECase
    <$> (reserved "case" *> expression)
    <*> (reserved "of" *> sepBy1 alternative comma)

alternative :: Parser Alternative
alternative =
  Alternative
    <$> identifier
    <*> many identifier
    <*> (reservedOp "->" *> expression)

atomicExpression :: Parser Expression
atomicExpression =
  try eConstant
    <|> try eIdent
    <|> try eOp
    <|> try (parens expression)

eApp :: Parser Expression
eApp = foldl1 EApp <$> many1 atomicExpression

eIdent :: Parser Expression
eIdent = EIdent <$> identifier

eOp :: Parser Expression
eOp = EIdent <$> opStr

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

parse :: SourceName -> String -> Either ParseError Program
parse = Text.Parsec.parse (contents program)
