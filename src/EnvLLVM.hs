module EnvLLVM where


import AbsInstant
import Control.Monad.State
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set


type Env = (Map.Map String Integer, Integer)

data ExpResult = Number Integer | Register Integer

type MonadExpResult = StateT Env IO ExpResult
type MonadValueResult = StateT Env IO Integer
type MonadEmptyResult = StateT Env IO ()
type MonadSetResult = StateT Env IO (Set.Set String)

showWithoutType :: ExpResult -> String
showWithoutType (Number value) = show value
showWithoutType (Register value) = "%t" ++ (show value)

showWithType :: ExpResult -> String
showWithType result = "i32 " ++ (showWithoutType result)

emptyEnv :: Env
emptyEnv = (Map.empty, 0)

createVarOrPass :: String -> Env -> Env
createVarOrPass varName (varEnv, counter) = 
  case Map.lookup varName varEnv of
    Just _ -> (varEnv, counter)
    Nothing -> (Map.insert varName counter varEnv, counter + 1)

saveVarToEnv :: String -> MonadEmptyResult
saveVarToEnv varName = state $ \s -> ((), createVarOrPass varName s)

getVarCounter :: String -> MonadValueResult
getVarCounter varName = state $ \s -> (getVarCounterFromEnv varName s, s)

getVarCounterFromEnv :: String -> Env -> Integer
getVarCounterFromEnv varName (varEnv, _) =
  case Map.lookup varName varEnv of
      Just value -> value
      Nothing -> error "Undefined variable"

getAvailableRegisterId :: MonadValueResult
getAvailableRegisterId = state $ \(varEnv, counter) -> (counter, (varEnv, counter + 1))

idEnv :: MonadEmptyResult
idEnv = state $ \s -> ((), s)

idEnvValue :: Integer -> MonadValueResult
idEnvValue value = state $ \s -> (value, s)

idEnvSet :: Set.Set String -> MonadSetResult
idEnvSet set = state $ \s -> (set, s)

idEnvExpResult :: ExpResult -> MonadExpResult
idEnvExpResult result = state $ \s -> (result, s)

io :: IO a -> StateT Env IO a
io = liftIO
