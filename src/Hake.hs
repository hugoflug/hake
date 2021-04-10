{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

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

-- TODO: switch to getArgs from unix package?
getArgsText :: IO [Text]
getArgsText = fmap pack <$> getArgs

interpretText :: (MonadInterpreter m, Typeable a) => Text -> a -> m a
interpretText t w = interpret (unpack t) w

decodeText :: Text -> Maybe Value
decodeText = decode . fromStrict . encodeUtf8

interpretCommand :: MonadInterpreter m => Text -> m (Value -> (Result (IO ())))
interpretCommand command = 
  interpretText ("fmap " <> command <> ". fromJSON") infer

desugar :: [Text] -> Maybe Value
desugar = \case
  [arg] ->
    case decodeText arg of
      Just value -> Just value
      Nothing -> desugarNoJson [arg]
  args ->
    desugarNoJson args

splitIn2 :: Text -> Text -> Maybe (Text, Text)
splitIn2 s t = case splitOn s t of
  [fst, snd] -> Just (fst, snd)
  _ -> Nothing 

desugarNoJson :: [Text] -> Maybe Value
desugarNoJson = 
  fmap (object . (fmap (second String))) . traverse (splitIn2 "=")

exit :: Text -> IO a
exit text = die $ unpack ("Error: " <> text) 

run :: IO ()
run = do
  args <- getArgsText
  (cmd, cmdArgs) <- case args of
    [] -> exit "No command specified"
    [_] -> exit "No arguments specified" 
    cmd:args -> pure (cmd, args)
  result <- runInterpreter $ do
    setImports ["Data.Aeson", "Data.Aeson.Types"] -- TODO: how are these loaded?
    set [languageExtensions := [DeriveGeneric, DeriveAnyClass]]

    -- TODO: support alternative module names
    loadModules ["Hakefile"]
    setTopLevelModules ["Hakefile"]    
    interpretCommand cmd

  case result of
    Left (WontCompile errs) ->
      traverse_ (putStrLn . errMsg) errs
    Left err -> error $ show err
    Right runCmd -> do
      decodedArg <- case desugar cmdArgs of
        Nothing -> exit "Invalid argument syntax"
        Just a -> pure a
      case runCmd decodedArg of
        Error str -> exit $ pack str
        Success action -> action