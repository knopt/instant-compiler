module EnvJVM where


import AbsInstant
import Control.Monad.State
import qualified Data.Map.Strict as Map


type Env = (Map.Map String Int, Int)

type MonadValueResult = StateT Env IO Int
type MonadEmptyResult = StateT Env IO ()
type MonadExpInfoResult = StateT Env IO (Int, String)

emptyEnv :: Env
emptyEnv = (Map.empty, 2)

createVarOrPass :: String -> Env -> Env
createVarOrPass varName (varEnv, counter) = 
  case Map.lookup varName varEnv of
    Just _ -> (varEnv, counter)
    Nothing -> (Map.insert varName counter varEnv, counter + 1)

saveVarToEnv :: String -> MonadEmptyResult
saveVarToEnv varName = state $ \s -> ((), createVarOrPass varName s)

getVarCounter :: String -> MonadValueResult
getVarCounter varName = state $ \s -> (getVarCounterFromEnv varName s, s)

getVarCounterFromEnv :: String -> Env -> Int
getVarCounterFromEnv varName (varEnv, _) =
  case Map.lookup varName varEnv of
      Just value -> value
      Nothing -> error "Undefined variable"

idEnv :: MonadEmptyResult
idEnv = state $ \s -> ((), s)

idEnvValue :: Int -> MonadValueResult
idEnvValue value = state $ \s -> (value, s)

idEnvExpInfo :: Int -> String -> MonadExpInfoResult
idEnvExpInfo height commands = state $ \s -> ((height, commands), s)

io :: IO a -> StateT Env IO a
io = liftIO
