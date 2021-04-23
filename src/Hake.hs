module Hake where

import Language.Haskell.Interpreter
import Data.Aeson (Result(..), Value(..), decode, object)
import Data.Bifunctor (second)
import Data.ByteString.Lazy (fromStrict)
import Data.Text (Text, unpack, pack, splitOn)
import Data.Text.Encoding (encodeUtf8)
import Data.Typeable (Typeable)

data Invocation = Invocation {
  cmdName :: Text,
  cmdArgs :: [Text],
  modules :: [Text]
} deriving (Eq, Show)

data Command =
  NoArgs (IO ()) |
  WithArgs (Value -> Result (IO ()))

data InvocationError =
  InvalidArgumentSyntax |
  InvalidArguments Text
  deriving Show 

interpretText :: (MonadInterpreter m, Typeable a) => Text -> a -> m a
interpretText t w = interpret (unpack t) w

interpretCommand :: MonadInterpreter m => Invocation -> m Command
interpretCommand Invocation{cmdName, cmdArgs, modules} = do
  setImports ["Data.Aeson"]
  set [languageExtensions := [DeriveGeneric, DeriveAnyClass]]

  loadModules $ unpack <$> modules
  setTopLevelModules $ unpack <$> modules
  case cmdArgs of
    [] -> NoArgs <$> interpretText cmdName infer
    _ -> WithArgs <$> interpretText ("fmap " <> cmdName <> ". fromJSON") infer

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

runCommand :: MonadInterpreter m => Invocation -> m (Either InvocationError (IO ()))
runCommand invocation = do
  command <- interpretCommand invocation
  pure $ case desugar (cmdArgs invocation) of
    Nothing -> Left InvalidArgumentSyntax
    Just desugaredArgs ->
      case command of
        NoArgs cmd -> Right cmd
        WithArgs runCmd ->
          case runCmd desugaredArgs of 
            Error str -> Left $ InvalidArguments $ pack str
            Success action -> Right action