module RunProgram (runProgram) where

import BuiltInFunctions
import Control.Monad
import Control.Monad.Error
import Control.Monad.State.Strict
import Data.IORef

import Absbrick
import RunUtils


evalExpresion :: Exp -> Exe BValue
evalExpresion ENone = return BVNone
evalExpresion (EAsign (CIdent ident) expr) = do
    value <- evalExpresion expr
    setVarIntoEnv ident value >> return value
evalExpresion (EYield expr) = evalExpresion expr >>= return . BVYield
evalExpresion ETrue = return $ BVBool True
evalExpresion EFalse = return $ BVBool True
evalExpresion (EFor (CIdent ident) beginExpr endExpr stmts) = do
    begin <- evalExpresion beginExpr >>= integerCast
    end <- evalExpresion endExpr >>= integerCast
    ys <- liftIO $ newIORef []
    inEnvironment $ do
        modify $ \s-> s {eYieldStatus = YSLoop ys}
        insertVarIntoEnv ident (BVInt begin)
        forLoop begin end ys
    where forLoop current end ys = if current < end
                                then do
                                    setVarIntoEnv ident (BVInt current)
                                    res <- runFunctionStatements stmts
                                    case res of
                                        BVReturn _ -> return res
                                        BVBreak value -> return  value
                                        BVContinue value -> do
                                            liftIO $ modifyIORef ys (value:)
                                            forLoop (current + 1) end ys
                                        _ -> forLoop (current + 1) end ys
                                else liftIO $ (readIORef ys) >>= return.BVList
evalExpresion (EWhile conditionExpr stmts) = do
    ys <- liftIO $ newIORef []
    inEnvironment $ do
        modify $ \s-> s {eYieldStatus = YSLoop ys}
        whileLoop ys
    where whileLoop ys = do
            cond <- evalExpresion conditionExpr >>= boolCast
            if cond
                then do
                    res <- runFunctionStatements stmts
                    case res of
                        BVReturn _ -> return res
                        BVBreak value -> return  value
                        BVContinue value -> do
                            liftIO $ modifyIORef ys (value:)
                            whileLoop ys
                        _ -> whileLoop ys
                else liftIO $ (readIORef ys) >>= return.BVList
evalExpresion (EInt value) = return $ BVInt value
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
    inEnvironment $ mapM_ (uncurry insertVarIntoEnv) (joinParams params values) >> runFunctionStatements stmts


runFunDeclaration :: FunDeclaration -> Exe ()
runFunDeclaration (FunDec (CIdent name) params stmts) =
    setFunIntoEnv name (makeExeFunction params stmts)


-- Przeleć liste i wykonuj runFunDeclaration
-- Wykonaj to co jest pod Main bez parametrów
runProgram :: Program -> IO (Either String Integer)
runProgram (Progr decls) = do
    let evalResult = inEnvironment $ mapM_ runFunDeclaration decls >> getFunFromEnv "Main" >>= \f -> f []
    p <- evalStateT (runErrorT evalResult) (Environment [] builtInFunctions YSFunction Nothing)
    case p of
        Left l -> return $ Left l
        Right (BVInt r) -> return $ Right r
        Right BVNone -> return $ Right 0
        _ -> return $ Left "RTE: Main returned not Int value." -- TODO: Dodać wypisanie lewej wartości?


-- Wykonuje akcję w nowym środowisku na obecnym
inEnvironment :: Exe a -> Exe a
inEnvironment action = do
    current <- get
    put $ Environment [] [] YSKeepLooking (Just current)
    result <- action
    put current
    return result

