module RunUtils where

import Control.Monad.Error
import Control.Monad.State.Strict
import Data.IORef
import System.IO ( stderr, hPutStrLn )


type Exe = ErrorT String (StateT Environment IO)
type ExeFunction = [BValue] -> Exe BValue


data YeldStatus
    = YSFunction
    | YSKeepLooking -- Jesteś gdzieś głębiej w pętli
    | YSLoop (IORef [BValue])


data Environment = Environment
    { eVar :: [(String, BValue)]
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


boolCast :: BValue -> Exe Bool
boolCast (BVBool value) = return value
boolCast (BVInt value) = warn "RTW: Casting from Int to Bool" >> return (value /= 0)
boolCast _ = throwError "RTE: Casting error"


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
getVarFromEnv = getFromEnv eVar


type Updater a = ([(String, a)] -> (String, a) -> Environment -> Environment)

setIntoEnv :: (Environment -> [(String, a)]) -> Updater a -> String -> a -> Exe ()
setIntoEnv selector update name value = do
    current <- get
    case lookup name (selector current) of
        Nothing -> do
            put $ update (selector current) (name, value) current
        Just fun -> do
            warn $ "RTW: You are going to override existing symbol:" ++ name
            put $ update (filter ((/= name).fst) (selector current)) (name, value) current


setFunIntoEnv :: String -> ExeFunction -> Exe ()
setFunIntoEnv = setIntoEnv eFun updateFunEnv where
    updateFunEnv record value obj = obj {eFun = value : record}


setVarIntoEnv :: String -> BValue -> Exe ()
setVarIntoEnv = setIntoEnv eVar updateVarEnv where
    updateVarEnv record value obj = obj {eVar = value : record}
