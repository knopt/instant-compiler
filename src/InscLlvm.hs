module Main where

import ExecuteLLVM

import System.Process
import System.FilePath
import System.Environment
import System.IO

main = do
  (fileName:_) <- getArgs
  compileLLVM fileName (dropExtension fileName)
  currPath <- getExecutablePath
  (runCommand $ "llvm-as -o " ++ 
                (replaceExtension fileName "bc") ++
                " " ++
                (replaceExtension fileName "ll"))
    >>= waitForProcess
  (runCommand $ "llvm-as -o " ++
                (joinPath [dropFileName currPath, "lib", "runtime.bc"]) ++
                " " ++
                (joinPath [dropFileName currPath, "lib", "runtime.ll"])) 
    >>= waitForProcess
  (runCommand $ "llvm-link -o " ++ 
                (replaceExtension fileName "bc") ++
                " " ++ 
                (replaceExtension fileName "bc") ++ 
                " " ++
                "lib/runtime.bc")
    >>= waitForProcess
