import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment (getArgs)
import Control.Monad (liftM)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

parseString :: Parser LispVal
parseString = do char '"'
                 x <- many (noneOf "\"")
                 char '"'
                 return $ String x

parseAtom :: Parser LispVal
parseAtom =
  do first <- letter <|> symbol
     rest <- many (letter <|> digit <|> symbol)
     let atom = first : rest
     return $ case atom of
                "#t" -> Bool True
                "#f" -> Bool False
                otherwise -> Atom atom

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

symbol :: Parser Char
symbol = oneOf "!$%$}*+-/:<=?>@^_~#"

spaces :: Parser ()
spaces = skipMany1 space

readExpr :: String -> String
readExpr input = case parse (spaces >> symbol) "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value: " ++ show val

main :: IO ()
main = do args <- getArgs
          putStrLn . readExpr . head $ args
