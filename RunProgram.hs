module RunProgram (runProgram) where

import Absbrick
import BuiltInFunctions
import Control.Monad.Error
import Control.Monad.State.Strict
import Data.IORef

type Exe = ErrorT String (StateT Environment IO)
type ExeFunction = [BValue] -> Exe BValue

data Environment = Environment 
    { eVar :: [(String, IORef BValue)]
    , eFun :: [(String, ExeFunction)]
    , eParent :: Maybe Environment
    }

{-data BType 
    = BTNone
    | BTInt
    | BTBool
    | BTString
    | BTList
    | BTDict-}
    
data BValue
    = BVNone
    | BVInt Int
    | BVBool Bool
    | BVString String
    | BVList [IORef BValue]
    | BVDict [(BValue, IORef BValue)]
    
  
  
runFunDeclaration :: FunDeclaration -> Exe ()
runFunDeclaration (FunDec ident params stmts) =
     undefined

     
-- Przeleć liste i wykonuj runFunDeclaration
-- Wykonaj to co jest pod Main bez parametrów
runProgram :: Program -> IO (Either String Int)
runProgram (Progr decls) = do
    let evalResult = inEnvironment $ mapM_ runFunDeclaration decls >> getFunFromEnv "Main" >>= \f -> f []
    p <- evalStateT (runErrorT evalResult) (Environment [] builtInFunctions Nothing)
    case p of
        Left l -> return $ Left l
        Right (BVInt r) -> return $ Right r
        _ -> return $ Left "RTE: Main returned not Int value." -- TODO: Dodać wypisanie lewej wartości?

      
-- Wykonuje akcję w nowym środowisku na obecnym
inEnvironment :: Exe a -> Exe a
inEnvironment action = do
    current <- get
    put $ Environment [] [] (Just current)
    result <- action
    put current
    return result
    
    
getFunFromEnv :: String -> Exe ExeFunction
getFunFromEnv name = do
    currentEnv <- get
    maybe (throwError $ "RTE: Cannot find function " ++ name ++ " in the scope.") return (getIt currentEnv) 
    where getIt env = case lookup name (eFun env) of
                          Just fun -> Just fun
                          Nothing -> eParent env >>= getIt
