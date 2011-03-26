import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment (getArgs)

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
