module RunProgram (runProgram) where

import BuiltInFunctions
import Control.Monad
import Control.Monad.Error
import Control.Monad.State.Strict
import Data.IORef

import Lexbrick
import Parbrick
import Absbrick
import RunUtils
import ErrM


evalExpresion :: Exp -> Exe BValue
evalExpresion ENone = return BVNone
evalExpresion (EFunNone _) = return BVNone
evalExpresion (EAssign (CIdent ident) expr) = do
    value <- evalExpresion expr
    updateVarIntoEnv ident value >> return value
evalExpresion (ELet (CIdent ident) expr) = do
    value <- evalExpresion expr
    declareVarIntoEnv ident value >> return value
evalExpresion (EYield expr) = evalExpresion expr >>= return . BVYield
evalExpresion ETrue = return $ BVBool True
evalExpresion EFalse = return $ BVBool False
evalExpresion (EFor (CIdent ident) beginExpr endExpr stmts) = do
    begin <- evalExpresion beginExpr >>= integerCast
    end <- evalExpresion endExpr >>= integerCast
    ys <- liftIO $ newIORef []
    inEnvironment $ do
        refenv <- get
        env <- liftIO $ readIORef refenv
        liftIO $ writeIORef refenv $ env {eYieldStatus = YSLoop ys}
        declareVarIntoEnv ident (BVInt begin)
        forLoop begin end ys
    where forLoop current end ys = if current <= end
                                then do
                                    declareVarIntoEnv ident (BVInt current)
                                    res <- runStatements stmts
                                    case res of
                                        BVReturn _ -> return res
                                        BVBreak value -> return  value
                                        BVContinue value -> do
                                            liftIO $ modifyIORef ys (value:)
                                            forLoop (current + 1) end ys
                                        _ -> forLoop (current + 1) end ys
                                else liftIO $ (readIORef ys) >>= return . BVList . reverse
evalExpresion (EWhile conditionExpr stmts) = do
    ys <- liftIO $ newIORef []
    inEnvironment $ do
        refenv <- get
        env <- liftIO $ readIORef refenv
        liftIO $ writeIORef refenv $ env {eYieldStatus = YSLoop ys}
        whileLoop ys
    where whileLoop ys = do
            cond <- evalExpresion conditionExpr >>= boolCast
            if cond
                then do
                    res <- runStatements stmts
                    case res of
                        BVReturn _ -> return res
                        BVBreak value -> return  value
                        BVContinue value -> do
                            liftIO $ modifyIORef ys (value:)
                            whileLoop ys
                        _ -> whileLoop ys
                else liftIO $ (readIORef ys) >>= return . BVList . reverse
evalExpresion (EInt value) = return $ BVInt value
evalExpresion (ENegInt value) = return $ BVInt (0-value)
evalExpresion (EString value) = return $ BVString value
evalExpresion (EIdent (CIdent ident)) = getVarFromEnv ident
evalExpresion (EFunPar nameExpr params) = do
    case nameExpr of
        EIdent (CIdent ident) -> do
            paramValues <- mapM evalExpresion params
            getFunFromEnv ident >>= \f-> f paramValues
        _ -> do
            nameValue <- evalExpresion nameExpr
            case nameValue of
                 BVBool condition -> do
                    when condition $ mapM_ evalExpresion params
                    return BVNone
                 BVString name -> do
                    paramValues <- mapM evalExpresion params
                    getFunFromEnv name >>= \f-> f paramValues
                 _ -> throwError "RTE: You are trying to call sth not callable."


runStatement :: Stm -> Exe BValue
runStatement (SIf cond ifStmts) = runStatement (SIfElse cond ifStmts [])
runStatement (SIfElse cond ifStmts elseStmts) = evalExpresion cond >>= boolCast >>=
    \value -> if value then  runStatements ifStmts else runStatements elseStmts
runStatement (SJump jumpStm) = runJumpStatement jumpStm
runStatement (SExp expr) = evalExpresion expr
runStatement (SFunDef def) = runFunDeclaration def >> return BVNone
runStatement (SExec stm) = do
    res <- runStatement stm
    case res of
        BVString str -> execString str
        _ -> throwError "RTE: Exec: You are trying to call sth not callable."


execString :: String -> Exe BValue
execString source = do
    case pProgram . myLexer $ source of
        Bad s -> throwError $ ("RTE: Exec: Parse failed: " ++ s)
        Ok (Progr stmts) -> do
            runStatements stmts


runJumpStatement :: JumpStm -> Exe BValue
runJumpStatement SjumpReturn = return $ BVReturn BVNone
runJumpStatement (SjumpReturnV expr) = evalExpresion expr >>= return . BVReturn
runJumpStatement SjumpBreak = return $ BVBreak BVNone
runJumpStatement (SjumpBreakV expr) = evalExpresion expr >>= return . BVBreak
runJumpStatement SjumpContinue = return $ BVContinue BVNone
runJumpStatement (SjumpContinueV expr) = evalExpresion expr >>= return . BVContinue


yieldValue :: BValue -> Exe ()
yieldValue value = get >>= getYieldField
    where getYieldField iorefenv = do
            env <- liftIO $ readIORef iorefenv
            case eYieldStatus env of
                YSFunction -> throwError "RTE: Yield used outside loop."
                YSLoop result -> liftIO $ modifyIORef result (value :)
                YSKeepLooking -> maybe (throwError "RTE: Yield used outside loop.") (getYieldField) (eParent env)


checkIfInLoop :: String -> Exe ()
checkIfInLoop name = get >>= getYieldField
    where getYieldField iorefenv = do
            env <- liftIO $ readIORef iorefenv
            case eYieldStatus env of
                YSFunction -> throwError $ "RTE: " ++ name ++ " used outside loop."
                YSLoop result -> return ()
                YSKeepLooking -> maybe (throwError ("RTE: " ++ name ++ " used outside loop.")) getYieldField (eParent env)


runStatements :: [Stm] -> Exe BValue
runStatements stmts = do
    inEnvironment $ runStm stmts
    where
        runStm [] = return BVNone
        runStm (h:t) = do
            result <- runStatement h
            case result of
                 BVReturn _ -> return result
                 BVContinue _ -> checkIfInLoop "Continue" >> return result
                 BVBreak _ -> checkIfInLoop "Break" >> return result
                 BVYield value -> yieldValue value >> runStm t
                 _ -> runStm t


runFunctionStatements :: [Stm] -> Exe BValue
runFunctionStatements stmts = do
    result <- runStatements stmts
    return $ case result of
        BVReturn value -> value
        _ -> BVNone


makeExeFunction :: [FunParam] -> [Stm] -> IORef Environment -> ExeFunction
makeExeFunction params stmts exeEnv = \values -> do
    let joinParams params values = zip (map (\(FunParam (CIdent s)) -> s) params) values
    currentEnv <- get
    put exeEnv
    result <- inEnvironment $ mapM_ (uncurry declareVarIntoEnv) (joinParams params values) >> runFunctionStatements stmts
    put currentEnv
    return result


runFunDeclaration :: FunDeclaration -> Exe ()
runFunDeclaration (FunDec (CIdent name) params stmts) = do
    current <- get
    declareFunIntoEnv name (makeExeFunction params stmts current)


-- Przeleć liste i wykonuj runFunDeclaration
-- Wykonaj to co jest pod Main bez parametrów
runProgram :: Program -> IO (Either String Integer)
runProgram (Progr program) = do
    let runStm stm = do
        case stm of
            (SFunDef decl) -> runFunDeclaration decl
            _ -> throwError $ "RTE: Not fun decl in global scope" ++ (show stm)
    let evalResult = inEnvironment $ mapM_ runStm program >> getFunFromEnv "Main" >>= \f -> f []
    envref <- (newIORef (Environment [] builtInFunctions YSFunction Nothing))
    p <- evalStateT (runErrorT evalResult) envref
    case p of
        Left l -> return $ Left l
        Right (BVInt r) -> return $ Right r
        Right BVNone -> return $ Right 0
        _ -> return $ Left "RTE: Main returned not Int value." -- TODO: Dodać wypisanie lewej wartości?


-- Wykonuje akcję w nowym środowisku na obecnym
inEnvironment :: Exe a -> Exe a
inEnvironment action = do
    current <- get
    envref <- liftIO $ id $ newIORef (Environment [] [] YSKeepLooking (Just current))
    put envref
    result <- action
    put current
    return result

