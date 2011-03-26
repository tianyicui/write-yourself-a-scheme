import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment (getArgs)
import Control.Monad (liftM)
import Control.Monad.Error

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

instance Show LispVal where
  show = showVal where
    showVal :: LispVal -> String
    showVal (String contents) = "\"" ++ contents ++ "\""
    showVal (Atom name) = name
    showVal (Number contents) = show contents
    showVal (Bool True) = "#t"
    showVal (Bool False) = "#f"
    showVal (List contents) = "(" ++ unwordsList contents ++ ")"
    showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

    unwordsList :: [LispVal] -> String
    unwordsList = unwords . map showVal

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> parseNumber
        <|> parseQuoted
        <|> do char '('
               x <- try parseList <|> parseDottedList
               char ')'
               return x
  where

    parseString :: Parser LispVal
    parseString = do
        char '"'
        x <- many (noneOf "\"")
        char '"'
        return $ String x

    parseAtom :: Parser LispVal
    parseAtom = do
        first <- letter <|> symbol
        rest <- many (letter <|> digit <|> symbol)
        let atom = first : rest
        return $ case atom of
                "#t" -> Bool True
                "#f" -> Bool False
                otherwise -> Atom atom

    parseNumber :: Parser LispVal
    parseNumber = liftM (Number . read) $ many1 digit

    parseList :: Parser LispVal
    parseList = liftM List $ sepBy parseExpr spaces

    parseDottedList :: Parser LispVal
    parseDottedList = do
        head <- endBy parseExpr spaces
        tail <- char '.' >> spaces >> parseExpr
        return $ DottedList head tail

    parseQuoted :: Parser LispVal
    parseQuoted = do
        char '\''
        x <- parseExpr
        return $ List [Atom "quote", x]

    symbol :: Parser Char
    symbol = oneOf "!$%&}*+-/:<=?>@^_~#|"

    spaces :: Parser ()
    spaces = skipMany1 space

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List [Atom "if", pred, conseq, alt]) =
    do result <- eval pred
       case result of
         Bool False -> eval alt
         otherwise -> eval conseq
eval (List (Atom func : args)) = apply func =<< mapM eval args
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args =
    maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
        ($ args)
        $ lookup func primitives
  where
    primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
    primitives = [("+", numericBinop (+)),
                  ("-", numericBinop (-)),
                  ("*", numericBinop (*)),
                  ("/", numericBinop div),
                  ("mod", numericBinop mod),
                  ("quotient", numericBinop quot),
                  ("remainder", numericBinop rem),
                  ("=", numBoolBinop (==)),
                  ("<", numBoolBinop (<)),
                  (">", numBoolBinop (>)),
                  ("/=", numBoolBinop (/=)),
                  (">=", numBoolBinop (>=)),
                  ("<=", numBoolBinop (<=)),
                  ("&&", boolBoolBinop (&&)),
                  ("||", boolBoolBinop (||)),
                  ("string=?", strBoolBinop (==)),
                  ("string?", strBoolBinop (>)),
                  ("string<=?", strBoolBinop (<=)),
                  ("string>=?", strBoolBinop (>=)),
                  ("car", car),
                  ("cdr", cdr),
                  ("cons", cons),
                  ("eq?", eqv),
                  ("eqv?", eqv)]

    numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
    numericBinop op singleVal@[_] =
        throwError $ NumArgs 2 singleVal
    numericBinop op params =
        return . Number . foldl1 op =<< mapM unpackNum params

    boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
    boolBinop unpacker op args =
        if length args /= 2
        then throwError $ NumArgs 2 args
        else do
            left <- unpacker $ head args
            right <- unpacker $ args !! 1
            return $ Bool $ left `op` right

    numBoolBinop = boolBinop unpackNum
    strBoolBinop = boolBinop unpackStr
    boolBoolBinop = boolBinop unpackBool

    unpackNum :: LispVal -> ThrowsError Integer
    unpackNum (Number n) = return n
    unpackNum (String n) =
        let parsed = reads n in
            if null parsed
              then throwError
                  $ TypeMismatch "number"
                  $ String n
              else return . fst $ head parsed
    unpackNum (List [n]) = unpackNum n
    unpackNum notNum = throwError $ TypeMismatch "number" notNum

    unpackStr :: LispVal -> ThrowsError String
    unpackStr (String s) = return s
    unpackStr (Number s) = return $ show s
    unpackStr (Bool s) = return $ show s
    unpackStr notString = throwError $ TypeMismatch "string" notString

    unpackBool :: LispVal -> ThrowsError Bool
    unpackBool (Bool b) = return b
    unpackBool notBool = throwError $ TypeMismatch "boolean" notBool

car :: [LispVal] -> ThrowsError LispVal
car [List (x : _)] = return x
car [DottedList (x : _) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (_ : xs)] = return $ List xs
cdr [DottedList [xs] x] = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x, List []] = return $ List [x]
cons [x, List xs] = return $ List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList

eqv :: [LispVal] -> ThrowsError LispVal
eqv [Bool arg1, Bool arg2] =
    return $ Bool $ arg1 == arg2
eqv [Number arg1, Number arg2] =
    return $ Bool $ arg1 == arg2
eqv [String arg1, String arg2] =
    return $ Bool $ arg1 == arg2
eqv [Atom arg1, Atom arg2] =
    return $ Bool $ arg1 == arg2
eqv [DottedList xs x, DottedList ys y] =
    eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [List arg1, List arg2] =
    return $ Bool $ (length arg1 == length arg2) &&
      all eqvPair (zip arg1 arg2)
        where eqvPair (x1, x2) =
                  case eqv [x1, x2] of
                    Left err -> False
                    Right (Bool val) -> val
eqv [_, _] = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

instance Show LispError where
    show (UnboundVar message varname) = message ++ ": " ++ varname
    show (BadSpecialForm message form) = message ++ ": " ++ show form
    show (NotFunction message func) = message ++ ": " ++ show func
    show (NumArgs expected found) = "Expected " ++ show expected
    show (TypeMismatch expected found) =
        "Invalid type: expected " ++ expected ++ ", found " ++ show found
    show (Parser parseErr) = "Parse error at " ++ show parseErr

instance Error LispError where
    noMsg = Default "An error has occurred"
    strMsg = Default

type ThrowsError = Either LispError

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val

main :: IO ()
main = do
    args <- getArgs
    let evaled = liftM show $ readExpr (head args) >>= eval
    putStrLn $ extractValue $ trapError evaled
