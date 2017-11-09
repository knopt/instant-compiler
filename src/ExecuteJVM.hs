module ExecuteJVM where

import LexInstant
import ParInstant
import AbsInstant
import CompilerJVM

import ErrM
import EnvJVM

import Control.Monad.State
import System.Environment
import System.FilePath
import System.IO

runProgramJVM input outputName fileHandle = do
  case pProgram (myLexer input) of
      Ok e -> evalStateT (startInterpret e outputName fileHandle) emptyEnv
      Bad e -> print e

compileJVM fileName outputName = do
    input <- readFile fileName
    fileHandle <- openFile (outputName ++ ".j") WriteMode
    runProgramJVM input (takeFileName outputName) fileHandle
    hClose fileHandle
