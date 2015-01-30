module Lambda.Parser where

import Data.Functor

import Control.Applicative

import Text.Parsec
import Text.Parsec.String

import Lambda

expressionParser :: Parser v -> Parser (Lambda v)
expressionParser p = spaces *> choices
  where
    choices = choice $ map try
        [ applicationParser p
        , parensParser p
        , lambdaParser p
        , variableParser p ]

atomicParser :: Parser v -> Parser (Lambda v)
atomicParser p = choices
  where
    choices = choice $ map try
        [ parensParser p
        , lambdaParser p
        , variableParser p
        ]

applicationParser :: Parser v -> Parser (Lambda v)
applicationParser p = chainl1 (spaces *> atomicParser p) (pure Application)

parensParser :: Parser v -> Parser (Lambda v)
parensParser p = spaces *> between (char '(') (char ')') (expressionParser p)

lambdaParser :: Parser v -> Parser (Lambda v)
lambdaParser p = Lambda <$> left <*> right
  where
    left  = char '\\' *> spaces *> p
    right = spaces *> char '.' *> spaces *> (expressionParser p)

variableParser :: Parser v -> Parser (Lambda v)
variableParser p = Variable <$> p

charExpressionParser :: Parser (Lambda Char)
charExpressionParser = expressionParser lower

-- vim: set ts=4 sts=4 sw=4 et:
