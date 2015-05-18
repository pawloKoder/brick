module RunUtils where

import Control.Monad.Error
import Control.Monad.State.Strict
import Data.IORef
import Data.List
import System.IO ( stderr, hPutStrLn )


type Exe = ErrorT String (StateT (IORef Environment) IO)
type ExeFunction = [BValue] -> Exe BValue


data YeldStatus
    = YSFunction
    | YSKeepLooking
    | YSLoop (IORef [BValue])


data Environment = Environment
    { eVar :: [(String, BValue)]
    , eFun :: [(String, ExeFunction)]
    , eYieldStatus :: YeldStatus
    , eParent :: Maybe (IORef Environment)
    }


data BValue
    = BVNone
    | BVInt Integer
    | BVBool Bool
    | BVString String
    | BVList [BValue]
    | BVDict [(BValue, BValue)]
    | BVReturn BValue
    | BVBreak BValue
    | BVContinue BValue
    | BVYield BValue
    deriving (Eq, Ord, Show)

toStr :: BValue -> String
toStr (BVInt a) = show a
toStr (BVBool a) = show a
toStr (BVString a) = id a
toStr (BVList l) = "[" ++ (intercalate ", " (map toStr l)) ++ "]"
toStr (BVDict l) = "{" ++ (intercalate ", " (map (\(x, y) -> (toStr x) ++ " -> "++ (toStr y)) l)) ++ "}"
toStr a = show a



boolCast :: BValue -> Exe Bool
boolCast (BVBool value) = return value
boolCast (BVInt value) = warn "RTW: Casting from Int to Bool" >> return (value /= 0)
boolCast a = throwError $ "RTE: Casting error (Expected bool) " ++ (show a)


integerCast :: BValue -> Exe Integer
integerCast (BVInt value) = return value
integerCast a = throwError $ "RTE: Casting error (Expected int) " ++ (show a)


stringCast :: BValue -> Exe String
stringCast (BVString value) = return value
stringCast a = throwError $ "RTE: Casting error (Expected string)" ++ (show a)


listCast :: BValue -> Exe [BValue]
listCast (BVList value) = return value
listCast a = throwError $ "RTE: Casting error (Expected list)" ++ (show a)


dictCast :: BValue -> Exe [(BValue, BValue)]
dictCast (BVDict value) = return value
dictCast a = throwError $ "RTE: Casting error (Expected dict)" ++ (show a)


warn :: String -> Exe ()
warn error = liftIO $ hPutStrLn stderr error


getFromEnv :: (Environment -> [(String, a)]) -> String -> Exe a
getFromEnv selector name = do
    currentEnv <- get
    maybefun <- getIt currentEnv
    maybe (throwError $ "RTE: Cannot find symbol " ++ name ++ " in the scope.") return maybefun
    where
        getIt maybeiorefenv = do
            env <- liftIO $ readIORef maybeiorefenv
            case lookup name (selector env) of
                            Just fun -> return $ Just fun
                            Nothing -> case eParent env of
                                               Just parent -> getIt parent
                                               Nothing -> return Nothing


getFunFromEnv :: String -> Exe ExeFunction
getFunFromEnv = getFromEnv eFun


getVarFromEnv :: String -> Exe BValue
getVarFromEnv = getFromEnv eVar


-- Inserts or update var.
declareFunIntoEnv :: String -> ExeFunction -> Exe ()
declareFunIntoEnv name fn = do
    tmp <- get
    current <- liftIO $ readIORef tmp
    liftIO $ writeIORef tmp $ current {eFun = (name, fn) : filter ((/= name).fst) (eFun current)}

-- Inserts or update var.
declareVarIntoEnv :: String -> BValue -> Exe ()
declareVarIntoEnv name value = do
    tmp <- get
    current <- liftIO $ readIORef tmp
    liftIO $ writeIORef tmp $ current {eVar = (name, value) : filter ((/= name).fst) (eVar current)}


-- Recursive update value or add new in current env
updateVarIntoEnv :: String -> BValue -> Exe ()
updateVarIntoEnv name value = do
    tmp <- get
    refenv <- updateIt (Just tmp)
    env <- liftIO $ readIORef refenv
    liftIO $ writeIORef refenv $ env {eVar = (name, value) : filter ((/= name).fst) (eVar env)}
    where updateIt maybeiorefenv = do
            case maybeiorefenv of
                 Just iorefenv -> do
                    env <- liftIO $ readIORef iorefenv
                    case lookup name (eVar env) of
                        Just var -> return iorefenv
                        Nothing -> updateIt (eParent env)
