module Main where

import ExecuteJVM

import System.Process
import System.FilePath
import System.Environment
import System.IO

main = do
  (fileName:_) <- getArgs
  compileJVM fileName (dropExtension fileName)
  currPath <- getExecutablePath
  (runCommand $ "java -jar " ++
                (joinPath [dropFileName currPath, "lib", "jasmin.jar"]) ++
                " -d " ++
                (takeDirectory fileName) ++
                " " ++
                (replaceExtension fileName "j")) 
    >>= waitForProcess