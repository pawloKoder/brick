module RunUtils where

import Control.Monad.Error
import Control.Monad.State.Strict
import Data.IORef
import Data.List
import System.IO ( stderr, hPutStrLn )


type Exe = ErrorT String (StateT Environment IO)
type ExeFunction = [BValue] -> Exe BValue


data YeldStatus
    = YSFunction
    | YSKeepLooking -- Jesteś gdzieś głębiej w pętli
    | YSLoop (IORef [BValue])


data Environment = Environment
    { eVar :: [(String, IORef BValue)]
    , eFun :: [(String, ExeFunction)]
    , eYieldStatus :: YeldStatus
    , eParent :: Maybe Environment
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
    maybe (throwError $ "RTE: Cannot find symbol " ++ name ++ " in the scope.") return (getIt currentEnv)
    where getIt env = case lookup name (selector env) of
                          Just fun -> Just fun
                          Nothing -> eParent env >>= getIt


getFunFromEnv :: String -> Exe ExeFunction
getFunFromEnv = getFromEnv eFun


getVarFromEnv :: String -> Exe BValue
getVarFromEnv name = getFromEnv eVar name >>= liftIO . readIORef


-- Inserts or update var.
declareVarIntoEnv :: String -> BValue -> Exe ()
declareVarIntoEnv name value = do
    current <- get
    case lookup name (eVar current) of
        Nothing -> do
            var <- liftIO $ newIORef value
            put $ current {eVar = (name, var) : (eVar current)}
        Just var -> liftIO $ modifyIORef var $ const value


-- Recursive update value or add new in current env
updateVarIntoEnv :: String -> BValue -> Exe ()
updateVarIntoEnv name value = do
    currentEnv <- get
    maybe (declareVarIntoEnv name value) (\v->liftIO $ modifyIORef v (const value)) (updateIt currentEnv)
    where updateIt env = case lookup name (eVar env) of
                          Just var -> Just var
                          Nothing -> eParent env >>= updateIt
