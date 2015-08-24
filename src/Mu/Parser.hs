module Mu.Parser where

import Text.ParserCombinators.Parsec

loneArgument :: Parser String
loneArgument = do
	s <- many1 $ noneOf "\n\r \""
	optional space
	return s

quotedArgument :: Parser String
quotedArgument = do
	s <- between (char '"') (char '"') 
	   $ many1 
	   $ noneOf "\n\r\""
	optional space
	return s

arguments :: Parser [String]
arguments = do
	hd <- loneArgument
	as <- many $  loneArgument
			  <|> quotedArgument
	return $ hd : as

parseArguments :: String -> Maybe [String]
parseArguments s =
	case parse arguments "" s of
		Left  _  -> Nothing 
		Right ls -> Just ls

parseArgumentsDebug :: String -> Either ParseError [String]
parseArgumentsDebug s = parse arguments "" s