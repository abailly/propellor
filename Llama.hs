{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

-- | Properties for running llama-server as a macOS launchd service.
module Llama where

import Base (OSDarwin)
import Propellor.Base (
  Property,
  combineProperties,
  props,
  requires,
  (&),
 )
import qualified Propellor.Property.Brew as Brew
import Propellor.Property.Darwin (Service (..), running)
import Propellor.Types.OS (Distribution (Darwin))

data LlamaModel = LocalModel FilePath | HuggingFaceModel String
  deriving (Show, Eq)

-- | Configuration for llama-server.
data LlamaConfig = LlamaConfig
  { llamaModel :: LlamaModel
  -- ^ Path to the GGUF model file
  , llamaEmbeddings :: Bool
  -- ^ Whether to expose the embedding endpoint
  , llamaPort :: Int
  -- ^ Port to listen on
  }
  deriving (Show, Eq)

-- | Default paths for llama-server logs.
llamaLogDir :: FilePath
llamaLogDir = "/var/log/llama"

-- | Service name for llama-server.
llamaServiceName :: String
llamaServiceName = "com.llama.server"

-- | Build the llama-server command arguments from configuration.
llamaServerArgs :: LlamaConfig -> [String]
llamaServerArgs cfg =
  ["/opt/homebrew/bin/llama-server"]
    ++ ["--port", show (llamaPort cfg)]
    ++ ["--host", "0.0.0.0"]
    ++ llamaModelArgs (llamaModel cfg)
    ++ embeddingArgs
 where
  embeddingArgs =
    if llamaEmbeddings cfg
      then ["--embedding"]
      else []
  llamaModelArgs = \case
    LocalModel path -> ["-m", path]
    HuggingFaceModel name -> ["-hf", name]

-- | Create a launchd Service for llama-server.
llamaService :: LlamaConfig -> Service
llamaService cfg =
  Service
    { svcName = llamaServiceName
    , svcProgram = llamaServerArgs cfg
    , svcWorkingDir = "/tmp"
    , svcStdoutPath = llamaLogDir ++ "/llama-server.log"
    , svcStderrPath = llamaLogDir ++ "/llama-server.err"
    }

-- | Ensures llama-server is installed and running as a launchd service.
--
-- Requires llama.cpp to be installed via Homebrew. The service will be
-- configured to start at boot and restart if it crashes.
--
-- Example usage:
--
-- > & Llama.serverRunning LlamaConfig
-- >     { llamaModel = "/Users/shared/models/llama-7b.gguf"
-- >     , llamaEmbeddings = True
-- >     , llamaPort = 8080
-- >     }
serverRunning :: LlamaConfig -> Property OSDarwin
serverRunning cfg =
  combineProperties ("llama-server running on port " ++ show (llamaPort cfg)) $
    props
      & running (llamaService cfg)
        `requires` Brew.installed "llama.cpp"
