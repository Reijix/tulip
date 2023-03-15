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
  Declaration 
    <$> identifier
    <*> (reserved "=" *> expression)

-- expressions
expression :: Parser Expression
expression =
        try eCase
        <|> try eLam
        <|> try eApp

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

eLam :: Parser Expression
eLam = 
  ELam 
    <$> (reservedOp "\\" *> identifier)
    <*> (dot *> expression)

atomicExpression :: Parser Expression
atomicExpression =
  try eConstant
    <|> try eIdent
    <|> try eOp
    <|> try eConstr
    <|> try (parens expression)

eApp :: Parser Expression
eApp = foldl1 EApp <$> many1 atomicExpression

eConstr :: Parser Expression
eConstr = 
  EConstr
    <$> (reserved "data" *> int)

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
