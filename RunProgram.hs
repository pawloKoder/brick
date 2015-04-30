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
   

warn :: String -> Exe ()
warn error = undefined


runVarDeclaration :: String -> BValue -> Exe ()
runVarDeclaration name value = undefined


runStatemets :: [Stm] -> Exe BValue
runStatemets stmts = undefined

  
makeExeFunction :: [FunParam] -> [Stm] -> ExeFunction
makeExeFunction params stmts = \values -> do
    let joinParams params values = zip (map (\(FunParam (CIdent s)) -> s) params) values
    inEnvironment $ mapM_ (uncurry runVarDeclaration) (joinParams params values) >> runStatemets stmts
  
  
runFunDeclaration :: FunDeclaration -> Exe ()
runFunDeclaration (FunDec (CIdent name) params stmts) = do
    current <- get
    let fn = (name, makeExeFunction params stmts)
    case lookup name (eFun current) of
        Nothing -> do
            warn $ "RTW: You are going to override existing function:" ++ name
            put $ current {eFun = fn : (eFun current)}
        Just fun -> do
            put $ current {eFun = fn : filter ((/= name).fst) (eFun current)}

     
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
