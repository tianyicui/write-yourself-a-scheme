{-# LANGUAGE ExistentialQuantification #-}
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment (getArgs)
import Control.Monad (liftM, unless)
import Control.Monad.Error
import IO hiding (try)
import Data.IORef
import Data.Maybe (isJust)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
             | Func {
                   params :: [String]
                 , vararg :: Maybe String
                 , body :: [LispVal]
                 , closure :: Env
                 }

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

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
    showVal (PrimitiveFunc _) = "<primitive>"
    showVal (Func {params = args, vararg = varargs, body = body, closure = env}) =
        "(lambda (" ++ unwords (map show args) ++
            (case varargs of
                Nothing -> ""
                Just arg -> " . " ++ arg) ++ ") ...)"

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
                _    -> Atom atom

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
    symbol = oneOf "!$%&|*+-/:<=?>@^_~#"

    spaces :: Parser ()
    spaces = skipMany1 space


eval :: Env -> LispVal -> IOThrowsError LispVal
eval _   val@(String _) = return val
eval _   val@(Number _) = return val
eval _   val@(Bool _) = return val
eval env (Atom id) = getVar env id
eval _   (List [Atom "quote", val]) = return val
eval env (List [Atom "if", pred, conseq, alt]) =
    do result <- eval env pred
       case result of
         Bool False -> eval env alt
         otherwise  -> eval env conseq
eval env (List [Atom "set!", Atom var, form]) =
    eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) =
    eval env form >>= defineVar env var
eval env (List (Atom "define" : List (Atom var : params) : body)) =
    makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params) varargs : body)) =
    makeVarargs varargs env params body >>= defineVar env var
eval env (List (Atom "lambda" : List params : body)) =
    makeNormalFunc env params body
eval env (List (Atom "lambda" : DottedList params varargs : body)) =
    makeVarargs varargs env params body
eval env (List (Atom "lambda" : varargs@(Atom _) : body)) =
    makeVarargs varargs env [] body
eval env (List (function : args)) = do
    func <- eval env function
    argVals <- mapM (eval env) args
    apply func argVals
eval _   badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

makeFunc varargs env params body =
    return $ Func (map show params) varargs body env
makeNormalFunc = makeFunc Nothing
makeVarargs = makeFunc . Just . show

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (Func params varargs body closure) args =
    if num params /= num args && varargs == Nothing
        then throwError $ NumArgs (num params) args
        else liftIO (bindVars closure $ zip params args) >>=
            bindVarArgs varargs >>= evalBody
    where
      remainingArgs = drop (length params) args
      num = toInteger . length
      evalBody env = liftM last $ mapM (eval env) body
      bindVarArgs arg env =
          case arg of
              Just argName -> liftIO $
                  bindVars env [(argName, List remainingArgs)]
              Nothing -> return env

primitiveBindings :: IO Env
primitiveBindings =
    nullEnv >>= flip bindVars (map makePrimitiveFunc primitives)
  where
    makePrimitiveFunc (var, func) = (var, PrimitiveFunc func)
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
                  ("eqv?", eqv),
                  ("equal?", equal)]

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

    unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
    unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
                 do unpacked1 <- unpacker arg1
                    unpacked2 <- unpacker arg2
                    return $ unpacked1 == unpacked2
            `catchError` const (return False)

    equal :: [LispVal] -> ThrowsError LispVal
    equal [arg1, arg2] = do
        primitiveEquals <-
          liftM or $ mapM (unpackEquals arg1 arg2)
            [AnyUnpacker unpackNum
            ,AnyUnpacker unpackStr
            ,AnyUnpacker unpackBool]
        eqvEquals <- eqv [arg1, arg2]
        return $ Bool
          (primitiveEquals || let (Bool x) = eqvEquals in x)
    equal badArgList = throwError $ NumArgs 2 badArgList

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
    show (NumArgs expected found) = "Wrong number of arguments: expected " ++ show expected
    show (TypeMismatch expected found) =
        "Invalid type: expected " ++ expected ++ ", found " ++ show found
    show (Parser parseErr) = "Parse error at " ++ show parseErr

instance Error LispError where
    noMsg = Default "An error has occurred"
    strMsg = Default

type ThrowsError = Either LispError

type IOThrowsError = ErrorT LispError IO

type Env = IORef [(String, IORef LispVal)]

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action =
    fmap extractValue $ runErrorT $ trapError action

isBound :: Env -> String -> IO Bool
isBound envRef var =
    fmap (isJust . lookup var) (readIORef envRef)

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do
    env <- liftIO $ readIORef envRef
    maybe (throwError $ UnboundVar "Getting an unbound variable" var)
          (liftIO . readIORef)
          (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do
    env <- liftIO $ readIORef envRef
    maybe (throwError $ UnboundVar "Setting an unbound variable" var)
          (liftIO . (`writeIORef` value))
          (lookup var env)
    return value

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
    alreadyDefined <- liftIO $ isBound envRef var
    if alreadyDefined
        then setVar envRef var value >> return value
        else liftIO $ do
            valueRef <- newIORef value
            env <- readIORef envRef
            writeIORef envRef ((var, valueRef) : env)
            return value

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings =
    readIORef envRef >>= extendEnv bindings >>= newIORef
  where
    extendEnv bindings env =
        liftM (++ env) (mapM addBinding bindings)
    addBinding (var, value) = do
        ref <- newIORef value
        return (var, ref)

nullEnv :: IO Env
nullEnv = newIORef []

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

main :: IO ()
main = do
    args <- getArgs
    case length args of
        0 -> runRepl
        1 -> runOne $ head args
        _ -> putStrLn "Program takes only 0 or 1 argument"
  where

    readExpr :: String -> ThrowsError LispVal
    readExpr input = case parse parseExpr "lisp" input of
        Left err -> throwError $ Parser err
        Right val -> return val

    flushStr :: String -> IO ()
    flushStr str = putStr str >> hFlush stdout

    readPrompt :: String -> IO String
    readPrompt prompt = flushStr prompt >> getLine

    until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
    until_ pred prompt action = do
        result <- prompt
        unless (pred result) $
            action result >> until_ pred prompt action

    evalAndPrint :: Env -> String -> IO ()
    evalAndPrint env expr =
        evalString env expr >>= putStrLn

    evalString :: Env -> String -> IO String
    evalString env expr =
        runIOThrows $ liftM show $ liftThrows (readExpr expr) >>= eval env

    runOne :: String -> IO ()
    runOne expr = primitiveBindings >>= flip evalAndPrint expr

    runRepl :: IO ()
    runRepl = primitiveBindings >>= until_ (== "quit") (readPrompt "Lisp>>>") . evalAndPrint
