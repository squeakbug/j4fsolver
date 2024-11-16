{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module LibSolver.Logic.FirstOrder.Parser where

import Data.Void (Void)
import Data.Text (Text, pack)

import Control.Monad.Combinators

import           Text.Megaparsec.Char
import           Text.Megaparsec            ((<?>))
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char.Lexer as L

import LibSolver.BoolExpr
import LibSolver.Logic.FirstOrder

type Parser = P.Parsec Void Text

instance Expr FOLExpr where
    parseExpr = parseFOL

instance Expr DefiniteClause where
    parseExpr str = parseFOL str >>= toDefiniteClause

-----------------------------------------------------------------------------

sc :: Parser ()
sc = L.space
  space1
  (L.skipLineComment "//")
  (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = P.between (symbol "(") (symbol ")")

-----------------------------------------------------------------------------

parseFOL :: String -> FOLExpr
parseFOL input = case parse expr "" input of
    Left _  -> Left ParseError
    Right x -> return (associate x)

expr :: Parser FOLExpr
expr = buildExpressionParser table term <?> "expression"

term :: Parser FOLExpr
term = parens expr
   <|> try parseForAll
   <|> try parseExists
   <|> try parsePred
   <|> parseVal
   <?> "T, F, Predicate, ForAll or Exists"

parseTerm :: Parser Term
parseTerm = try parseFunc
        <|> parseVar
        <|> parseSym
        <?> "Variable, Symbol or Function"

parseVar :: Parser Term
parseVar = do
    Var <$> name

parseSym :: Parser Term
parseSym = do
    Sym <$> capsName

parseFunc :: Parser Term
parseFunc = do
    x  <- capsName
    ts <- parens (parseTerm `sepBy` char ',')
    return $ Func x ts

parseVal :: Parser FOLExpr
parseVal = (char 'T' >> return (Val True))
       <|> (char 'F' >> return (Val False))
       <?> "T or F"

parsePred :: Parser FOLExpr
parsePred = do
    x  <- capsName
    ts <- parens (parseTerm `sepBy` char ',')
    return $ Pred x ts

parseForAll :: Parser FOLExpr
parseForAll = do
    string "forall" >> spaces
    x <- name
    char '.' >> spaces
    ForAll x <$> expr

parseExists :: Parser FOLExpr
parseExists = do
    string "exists" >> spaces
    x <- name
    char '.' >> spaces
    Exists x <$> expr

capsName :: Parser String
capsName = do
    c  <- upper
    cs <- many alphaNum
    return (c:cs)

name :: Parser String
name = do
    c  <- lower
    cs <- many alphaNum
    return (c:cs)

table =
    [ [prefix "~" Not]
    , [binary "&" (\x y -> And [x,y]) AssocLeft]
    , [binary "|" (\x y -> Or [x,y]) AssocLeft]
    , [binary "=>" Implies AssocRight] ]

binary name fun = Infix (do { string name; return fun })
prefix name fun = Prefix (do { string name; return fun })
