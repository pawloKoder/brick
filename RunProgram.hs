module RunProgram (runProgram) where

import System.IO ( stderr, hPutStrLn )
import Absbrick
import BuiltInFunctions
import Control.Monad.Error
import Control.Monad.State.Strict
import Data.IORef

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
    | BVList [BValue]
    | BVDict [(BValue, BValue)]
    | BVReturn BValue
    | BVBreak BValue
    | BVContinue BValue
    | BVYield BValue

   

warn :: String -> Exe ()
warn error = liftIO $ hPutStrLn stderr error


runVarDeclaration :: String -> BValue -> Exe ()
runVarDeclaration name value = do
    modify $ \s -> s {eVar = ((name, value) : filter ((/= name).fst) (eVar s)) }

    
evalExpresion :: Exp -> Exe BValue
evalExpresion = undefined


boolCast :: BValue -> Exe Bool
boolCast = undefined


runStatement :: Stm -> Exe BValue
runStatement (SIf cond ifStmts) = runStatement (SIfElse cond ifStmts [])
runStatement (SIfElse cond ifStmts elseStmts) = evalExpresion cond >>= boolCast >>=
    \value -> if value then  runStatements ifStmts else runStatements elseStmts
runStatement (SJump jumpStm) = runJumpStatement jumpStm
runStatement (SExp expr) = evalExpresion expr
runStatement (SFunDef def) = runFunDeclaration def >> return BVNone


runJumpStatement :: JumpStm -> Exe BValue
runJumpStatement SjumpReturn = return $ BVReturn BVNone
runJumpStatement (SjumpReturnV expr) = evalExpresion expr >>= return . BVReturn
runJumpStatement SjumpBreak = return $ BVBreak BVNone
runJumpStatement (SjumpBreakV expr) = evalExpresion expr >>= return . BVBreak
runJumpStatement SjumpContinue = return $ BVContinue BVNone
runJumpStatement (SjumpContinueV expr) = evalExpresion expr >>= return . BVContinue


yieldValue :: BValue -> Exe ()
yieldValue value = get >>= getYieldField
    where getYieldField env =
            case eYieldStatus env of
                YSFunction -> throwError "RTE: Yield used outside loop."
                YSLoop result -> liftIO $ modifyIORef result (value :)
                YSKeepLooking -> maybe (throwError "RTE: Yield used outside loop.") getYieldField (eParent env)
         

  
runStatements :: [Stm] -> Exe BValue
runStatements stmts = do
    inEnvironment $ runStm stmts
    where
        runStm [] = return BVNone
        runStm (h:t) = do
            result <- runStatement h
            case result of
                 BVReturn _ -> return result
                 BVContinue _ -> return result
                 BVBreak _ -> return result
                 BVYield value -> yieldValue value >> runStm t
                 _ -> runStm t
  
  
runFunctionStatements :: [Stm] -> Exe BValue
runFunctionStatements stmts = do
    result <- runStatements stmts
    return $ case result of
        BVReturn value -> value
        _ -> BVNone
    
  
makeExeFunction :: [FunParam] -> [Stm] -> ExeFunction
makeExeFunction params stmts = \values -> do
    let joinParams params values = zip (map (\(FunParam (CIdent s)) -> s) params) values
    inEnvironment $ mapM_ (uncurry runVarDeclaration) (joinParams params values) >> runFunctionStatements stmts
  
  
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
    p <- evalStateT (runErrorT evalResult) (Environment [] builtInFunctions YSFunction Nothing)
    case p of
        Left l -> return $ Left l
        Right (BVInt r) -> return $ Right r
        _ -> return $ Left "RTE: Main returned not Int value." -- TODO: Dodać wypisanie lewej wartości?

      
-- Wykonuje akcję w nowym środowisku na obecnym
inEnvironment :: Exe a -> Exe a
inEnvironment action = do
    current <- get
    put $ Environment [] [] YSKeepLooking (Just current)
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
