module ExecuteLLVM where

import LexInstant
import ParInstant
import AbsInstant
import CompilerLLVM

import ErrM
import EnvLLVM

import Control.Monad.State
import System.Environment
import System.IO

runProgramLLVM input fileHandle = do
  case pProgram (myLexer input) of
      Ok e -> evalStateT (startInterpret e fileHandle) emptyEnv
      Bad e -> print e

compileLLVM fileName outputName = do
      input <- readFile fileName
      fileHandle <- openFile (outputName ++ ".ll") WriteMode
      runProgramLLVM input fileHandle
      hClose fileHandle
