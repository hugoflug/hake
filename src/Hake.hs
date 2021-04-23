module Hake where

import Language.Haskell.Interpreter
import System.Environment (getArgs)
import System.Exit (die)
import Data.Aeson (Result(..), Value(..), decode, object)
import Data.Bifunctor (second)
import Data.ByteString.Lazy (fromStrict)
import Data.Foldable (traverse_)
import Data.Text (Text, unpack, pack, splitOn)
import Data.Text.Encoding (encodeUtf8)
import Data.Typeable (Typeable)

data Command =
  NoArgs (IO ()) |
  WithArgs (Value -> Result (IO ()))

interpretText :: (MonadInterpreter m, Typeable a) => Text -> a -> m a
interpretText t w = interpret (unpack t) w

interpretCommand :: Text -> [Text] -> Text -> IO (Either InterpreterError Command)
interpretCommand cmd args moduleName = runInterpreter $ do
  setImports ["Data.Aeson"]
  set [languageExtensions := [DeriveGeneric, DeriveAnyClass]]

  loadModules [unpack moduleName]
  setTopLevelModules [unpack moduleName] 
  case args of
    [] -> NoArgs <$> interpretText cmd infer
    _ -> WithArgs <$> interpretText ("fmap " <> cmd <> ". fromJSON") infer

splitIn2 :: Text -> Text -> Maybe (Text, Text)
splitIn2 s t = case splitOn s t of
  [fstHalf, sndHalf] -> Just (fstHalf, sndHalf)
  _ -> Nothing 

desugarNoJson :: [Text] -> Maybe Value
desugarNoJson = 
  fmap (object . (fmap (second String))) . traverse (splitIn2 "=")

decodeText :: Text -> Maybe Value
decodeText = decode . fromStrict . encodeUtf8

desugar :: [Text] -> Maybe Value
desugar = \case
  [arg] ->
    case decodeText arg of
      Just value -> Just value
      Nothing -> desugarNoJson [arg]
  args ->
    desugarNoJson args

exit :: Text -> IO a
exit text = die $ unpack ("Error: " <> text) 

-- TODO: switch to getArgs from unix package?
getArgsText :: IO [Text]
getArgsText = fmap pack <$> getArgs

run :: IO ()
run = do
  programArgs <- getArgsText
  (cmd, cmdArgs) <- case programArgs of
    [] -> exit "No command specified"
    cmd:cmdArgs -> pure (cmd, cmdArgs)
  -- TODO: support alternative module names
  result <- interpretCommand cmd cmdArgs "Hakefile"
  case result of
    Left err ->
      case err of
        WontCompile errs -> do
          putStrLn $ "Errors in evaluated module(s):"
          traverse_ (putStrLn . errMsg) errs -- TODO: why are errors duplicated?
        otherError -> exit $ pack $ show otherError
    Right command -> do
      desugaredArgs <- case desugar cmdArgs of
        Nothing -> exit "Invalid argument syntax"
        Just a -> pure a
      case command of
        NoArgs c -> c
        WithArgs runC ->
          case runC desugaredArgs of 
            Error str -> exit $ pack str
            Success action -> action

