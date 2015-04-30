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
    | BVInt Int
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


getFunFromEnv :: String -> Exe ExeFunction
getFunFromEnv name = do
    currentEnv <- get
    maybe (throwError $ "RTE: Cannot find function " ++ name ++ " in the scope.") return (getIt currentEnv) 
    where getIt env = case lookup name (eFun env) of
                          Just fun -> Just fun
                          Nothing -> eParent env >>= getIt