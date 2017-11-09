module InterpreterJVM where

import AbsInstant
import ErrM
import EnvJVM
import System.IO

import qualified Data.Set as Set

getAddOp = "  iadd\n"
getSubOp = "  isub\n"
getMulOp = "  imul\n"
getDivOp = "  idiv\n"
getSwapOp = "  swap\n"
getStreamOut = "  getstatic java/lang/System/out Ljava/io/PrintStream;\n"
getPrintInt = "  invokevirtual java/io/PrintStream/println(I)V\n"

initializeClass :: String -> Handle -> MonadEmptyResult
initializeClass className = printValue $ 
  ".class public " ++ className ++ "\n\
  \.super java/lang/Object\n\n\
  \.method public <init>()V\n\
  \  aload_0\n\
  \  invokespecial java/lang/Object/<init>()V\n\
  \  return\n\
  \.end method\n"

getConstant :: Integer -> String
getConstant value = 
  if value <= 5
    then "  iconst_" ++ (show value) ++ "\n"
    else if value <= 127
      then "  bipush " ++ (show value) ++ "\n"
      else if value <= 32767
        then "  sipush " ++ (show value) ++ "\n"
        else "  ldc " ++ (show value) ++ "\n"

getVarValue :: Int -> String
getVarValue id = "  iload " ++ (show id) ++ "\n"

getStoreOp :: Int -> String
getStoreOp id = "  istore " ++ (show id) ++ "\n"

printValue :: String -> Handle -> MonadEmptyResult
printValue value handle = io $ hPutStrLn handle value

printValueWithIndent :: String -> Handle -> MonadEmptyResult
printValueWithIndent value = printValue $ "  " ++ value

startInterpret :: Program -> String ->Handle -> MonadEmptyResult
startInterpret x outputName handle = do
  (h, cmds) <- interpretProgram x 0 ""
  initializeClass outputName handle
  printValue ".method public static main([Ljava/lang/String;)V" handle
  printValueWithIndent (".limit stack " ++ (show h))  handle
  printValueWithIndent (".limit locals " ++ (show $ (countLocals x) + 2)) handle
  printValue cmds handle
  printValueWithIndent "return" handle
  printValue ".end method" handle

getLocalsR :: Program -> Set.Set String -> Set.Set String
getLocalsR (Prog []) locals = locals 
getLocalsR (Prog (stmt:stmts)) locals = case stmt of
  SAss (Ident ident) _ -> getLocalsR (Prog stmts) (Set.insert ident locals)
  SExp _ -> getLocalsR (Prog stmts) locals

countLocals :: Program -> Int
countLocals prog = Set.size $ getLocalsR prog Set.empty

interpretProgram :: Program -> Int -> String -> MonadExpInfoResult
interpretProgram x maxVal prevCmds =
  case x of
    Prog (stmt:stmts) -> do
      (stackHeight, cmds) <- interpretStmt stmt
      interpretProgram (Prog stmts) (max maxVal stackHeight) (prevCmds ++ "\n" ++ cmds)
    Prog [] -> do
      idEnvExpInfo maxVal prevCmds

interpretStmt :: Stmt -> MonadExpInfoResult
interpretStmt x = case x of
  SExp exp -> do
    (h, cmds) <- interpretExp exp
    idEnvExpInfo (h + 2) (getStreamOut ++ cmds ++ getPrintInt)
  SAss (Ident ident) exp -> do
    saveVarToEnv ident
    id <- getVarCounter ident
    (h, cmds) <- interpretExp exp
    idEnvExpInfo (h + 1) (cmds ++ (getStoreOp id))


interpretExp :: Exp -> MonadExpInfoResult
interpretExp x  = case x of
  ExpAdd exp1 exp2 -> do
    (h1, c1) <- interpretExp exp1
    (h2, c2) <- interpretExp exp2
    if not (h2 > h1)
      then idEnvExpInfo (max h1 (h2 + 1)) (c1 ++ c2 ++ getAddOp)
      else idEnvExpInfo (max h2 (h1 + 1)) (c2 ++ c1 ++ getAddOp)
  ExpSub exp1 exp2 -> do
    (h1, c1) <- interpretExp exp1
    (h2, c2) <- interpretExp exp2
    if not (h2 > h1)
      then idEnvExpInfo (max h1 (h2 + 1)) (c1 ++ c2 ++ getSubOp)
      else idEnvExpInfo (max h2 (h1 + 1)) (c2 ++ c1 ++ getSwapOp ++ getSubOp)
  ExpMul exp1 exp2 -> do
    (h1, c1) <- interpretExp exp1
    (h2, c2) <- interpretExp exp2
    if not (h2 > h1)
      then idEnvExpInfo (max h1 (h2 + 1)) (c1 ++ c2 ++ getMulOp)
      else idEnvExpInfo (max h2 (h1 + 1)) (c2 ++ c1 ++ getMulOp)
  ExpDiv exp1 exp2 -> do
    (h1, c1) <- interpretExp exp1
    (h2, c2) <- interpretExp exp2
    if not (h2 > h1)
      then idEnvExpInfo (max h1 (h2 + 1)) (c1 ++ c2 ++ getDivOp)
      else idEnvExpInfo (max h2 (h1 + 1)) (c2 ++ c1 ++ getSwapOp ++ getDivOp)
  ExpLit integer -> do
    idEnvExpInfo 1 (getConstant integer)
  ExpVar (Ident ident) -> do
    id <- getVarCounter ident
    idEnvExpInfo 1 (getVarValue id)

