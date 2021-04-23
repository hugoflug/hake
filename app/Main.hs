import Hake

import Data.Text (Text, pack, unpack)
import Language.Haskell.Interpreter (InterpreterError(..), runInterpreter, errMsg)
import System.Exit (die, exitWith, ExitCode(..))
import Data.Foldable (traverse_)
import Options.Applicative

exit :: Text -> IO a
exit text = die $ unpack ("Error: " <> text) 

commandOption :: Parser Text
commandOption =
  strArgument $
    metavar "COMMAND"
    <> help "Command to run"

argumentOption :: Parser Text
argumentOption =
  strArgument $
    metavar "ARGUMENT"
      <> help "Argument to the command"

moduleOption :: Parser Text
moduleOption = 
  strOption $
    long "module"
    <> short 'm'
    <> metavar "MODULE"
    <> help "Module to run"

emptyTo :: a -> [a] -> [a]
emptyTo def = \case
  [] -> [def]
  l -> l

invocationParser :: Parser Invocation
invocationParser =
  Invocation 
    <$> commandOption
    <*> many argumentOption
    <*> (emptyTo "Hakefile" <$> many moduleOption)

opts :: ParserInfo Invocation
opts = info (invocationParser <**> helper)
  ( fullDesc
  <> progDesc "Run functions in Haskell modules" )

main :: IO ()
main = do
  invocation <- execParser opts
  result <- runInterpreter $ runCommand invocation
  case result of
    Left err -> 
      case err of
        WontCompile errs -> do
          putStrLn $ "Errors in evaluated module(s):"
          traverse_ (putStrLn . errMsg) errs -- TODO: why are errors duplicated?
          exitWith $ ExitFailure 1
        otherError -> exit $ pack $ show otherError
    Right (Left err) ->
      case err of
        InvalidArgumentSyntax ->
          exit "Invalid argument syntax"
        InvalidArguments txt ->
          exit txt
    Right (Right res) -> res

