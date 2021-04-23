import Hake

import Data.Text (Text, pack, unpack)
import Language.Haskell.Interpreter (InterpreterError(..), runInterpreter, errMsg)
import System.Environment (getArgs)
import System.Exit (die)
import Data.Foldable (traverse_)

exit :: Text -> IO a
exit text = die $ unpack ("Error: " <> text) 

-- TODO: switch to getArgs from unix package?
getArgsText :: IO [Text]
getArgsText = fmap pack <$> getArgs

main :: IO ()
main = do
  programArgs <- getArgsText
  (cmd, cmdArgs) <- case programArgs of
    [] -> exit "No command specified"
    cmd:cmdArgs -> pure (cmd, cmdArgs)
  result <- runInterpreter $ runCommand (Invocation cmd cmdArgs ["Hakefile"])
  case result of
    Left err -> 
      case err of
        WontCompile errs -> do
          putStrLn $ "Errors in evaluated module(s):"
          traverse_ (putStrLn . errMsg) errs -- TODO: why are errors duplicated?
        otherError -> exit $ pack $ show otherError
    Right (Left err) ->
      case err of
        InvalidArgumentSyntax ->
          exit "Invalid argument syntax"
        InvalidArguments txt ->
          exit txt
    Right (Right res) -> res

