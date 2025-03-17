module ParseTM where

import Text.Parsec

pRulesKeyword :: Parsec String () String
pRulesKeyword = string "Rules:"

pInput :: Parsec String () String
pInput = do
    spaces
    string "Input:"
    spaces
    s <- many1 alphaNum
    return s

