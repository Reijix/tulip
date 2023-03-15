module Lexer where

import Text.Parsec.Char
import Text.Parsec.Language
import Text.Parsec.String
import qualified Text.Parsec.Token as Tok

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
  where
    ops = ["->", "\\", "="]
    names = ["case", "of", "data"]
    style =
      emptyDef
        { Tok.commentLine = "#",
          Tok.reservedOpNames = ops,
          Tok.reservedNames = names,
          Tok.opLetter = oneOf ">=\\."
        }

int :: Parser Int
int = fromInteger <$> Tok.integer lexer

float :: Parser Double
float = Tok.float lexer

char :: Parser Char
char = Tok.charLiteral lexer

charLiteral :: Parser Char
charLiteral = Tok.charLiteral lexer

parens :: Parser a -> Parser a
parens = Tok.parens lexer

braces :: Parser a -> Parser a
braces = Tok.braces lexer

brackets :: Parser a -> Parser a
brackets = Tok.brackets lexer

commaSep :: Parser a -> Parser [a]
commaSep = Tok.commaSep lexer

semiSep :: Parser a -> Parser [a]
semiSep = Tok.semiSep lexer

semi :: Parser String
semi = Tok.semi lexer

colon :: Parser String
colon = Tok.colon lexer

dot :: Parser String
dot = Tok.dot lexer

comma :: Parser String
comma = Tok.comma lexer

identifierStr :: Parser String
identifierStr = Tok.identifier lexer

opStr :: Parser String
opStr = Tok.operator lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer
