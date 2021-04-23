module Hakefile where

import GHC.Generics (Generic)
import System.Process
import Data.Aeson

newtype Service = 
  Service String
  deriving (Generic, FromJSON, Show)

newtype Version =
  Version String
  deriving (Generic, FromJSON, Show)

data Environment =
  EuStaging |
  UsStaging |
  EuProduction |
  UsProduction 
  deriving (Generic, FromJSON, Show)

data Deploy =
  Deploy {
    service :: Service,
    environment :: Environment,
    version :: Version
  } deriving (Generic, FromJSON, Show)

prepare :: IO ()
prepare = callCommand "echo preparing"

deploy :: Deploy -> IO ()
deploy cmd = do
  callCommand $ "echo deploying: " <> show cmd