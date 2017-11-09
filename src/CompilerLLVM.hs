module InterpreterLLVM where

import AbsInstant
import ErrM
import EnvLLVM
import System.IO

import qualified Data.Set as Set

initializeProgram :: Handle -> MonadEmptyResult
initializeProgram = printValue $ "declare void @printInt(i32)"

yieldAlloc :: ExpResult -> Handle -> MonadEmptyResult
yieldAlloc (Register registerId) = printValueWithIndent $ (showWithoutType (Register registerId)) ++ " = alloca i32"

yieldStore :: ExpResult -> Integer -> Handle -> MonadEmptyResult
yieldStore expResult registerId = printValueWithIndent $ "store " ++ (showWithType expResult)
                                                        ++ ", i32* %t" ++ (show registerId)

yieldLoad :: Integer -> Integer -> Handle -> MonadEmptyResult
yieldLoad registerFrom registerTo = printValueWithIndent $ "%t" ++ (show registerTo) 
                                                            ++ " = load i32, i32* %t" ++ (show registerFrom)

yieldAdd :: Integer -> ExpResult -> ExpResult -> Handle -> MonadEmptyResult
yieldAdd = yieldStandardOp "add"

yieldSub :: Integer -> ExpResult -> ExpResult -> Handle -> MonadEmptyResult
yieldSub = yieldStandardOp "sub"

yieldMul :: Integer -> ExpResult -> ExpResult -> Handle -> MonadEmptyResult
yieldMul = yieldStandardOp "mul"

yieldDiv :: Integer -> ExpResult -> ExpResult -> Handle -> MonadEmptyResult
yieldDiv = yieldStandardOp "sdiv"

yieldStandardOp :: String -> Integer -> ExpResult -> ExpResult -> Handle -> MonadEmptyResult
yieldStandardOp op registerTo first second = printValueWithIndent $ "%t" ++ (show registerTo)
    ++ " = " ++ op ++ " i32 " ++ (showWithoutType first) ++ ", " ++ (showWithoutType second)

yieldPrint :: ExpResult -> Handle -> MonadEmptyResult
yieldPrint result = printValueWithIndent $ "call void @printInt(" ++ (showWithType result) ++ ")"

yieldEmptyLine :: Handle -> MonadEmptyResult
yieldEmptyLine = printValue $ ""

printValue :: String -> Handle -> MonadEmptyResult
printValue value handle = io $ hPutStrLn handle value

printValueWithIndent :: String -> Handle -> MonadEmptyResult
printValueWithIndent value = printValue $ "  " ++ value

startInterpret :: Program -> Handle -> MonadEmptyResult
startInterpret x handle = do
  initializeProgram handle
  printValue "define i32 @main() {" handle
  interpretWithAllocs Set.empty x x handle
  printValueWithIndent "ret i32 0" handle
  printValue "}" handle

interpretWithAllocs :: Set.Set String -> Program -> Program -> Handle -> MonadEmptyResult
interpretWithAllocs alreadyDeclared left all handle =
  case left of
    Prog (stmt:stmts) -> do
      resultDeclared <- allocVariable alreadyDeclared stmt handle
      interpretWithAllocs resultDeclared (Prog stmts) all handle
    Prog [] -> do
      interpretProgram all handle


allocVariable :: Set.Set String -> Stmt -> Handle -> MonadSetResult
allocVariable alreadyDeclared stmt handle = case stmt of
  SAss (Ident ident) _ -> 
    if Set.member ident alreadyDeclared
      then do idEnvSet alreadyDeclared
      else do
        saveVarToEnv ident
        registerId <- getVarCounter ident
        yieldAlloc (Register registerId) handle
        idEnvSet (Set.insert ident alreadyDeclared)
  SExp _ -> do
    idEnvSet alreadyDeclared

interpretProgram :: Program -> Handle -> MonadEmptyResult
interpretProgram x handle =
  case x of
    Prog (stmt:stmts) -> do
      interpretStmt stmt handle
      yieldEmptyLine handle
      interpretProgram (Prog stmts) handle
    Prog [] -> do
      idEnv

interpretStmt :: Stmt -> Handle -> MonadEmptyResult
interpretStmt x handle = case x of
  SExp exp -> do
    res <- interpretExp exp handle
    yieldPrint res handle

    idEnv
  SAss (Ident ident) exp -> do
    id <- getVarCounter ident
    res <- interpretExp exp handle
    yieldStore res id handle
    idEnv


interpretExp :: Exp -> Handle -> MonadExpResult
interpretExp x handle = case x of
  ExpAdd exp1 exp2 -> do
    res1 <- interpretExp exp1 handle
    res2 <- interpretExp exp2 handle
    destRegister <- getAvailableRegisterId
    yieldAdd destRegister res1 res2 handle
    idEnvExpResult $ Register destRegister
  ExpSub exp1 exp2 -> do
    res1 <- interpretExp exp1 handle
    res2 <- interpretExp exp2 handle
    destRegister <- getAvailableRegisterId
    yieldSub destRegister res1 res2 handle
    idEnvExpResult $ Register destRegister
  ExpMul exp1 exp2 -> do
    res1 <- interpretExp exp1 handle
    res2 <- interpretExp exp2 handle
    destRegister <- getAvailableRegisterId
    yieldMul destRegister res1 res2 handle
    idEnvExpResult $ Register destRegister
    
  ExpDiv exp1 exp2 -> do
    res1 <- interpretExp exp1 handle
    res2 <- interpretExp exp2 handle
    destRegister <- getAvailableRegisterId
    yieldDiv destRegister res1 res2 handle
    idEnvExpResult $ Register destRegister
  ExpLit integer -> do
    idEnvExpResult $ Number integer
  ExpVar (Ident ident) -> do
    varId <- getVarCounter ident
    destRegister <- getAvailableRegisterId
    yieldLoad varId destRegister handle
    idEnvExpResult $ Register destRegister

