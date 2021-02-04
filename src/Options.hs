module Options where

import Data.Time.Clock (NominalDiffTime)
import GHC.Generics (Generic)

data WatchDepth
  = Single
  | Recursive
  deriving (Eq, Show)

data Options = Options
  { watchDirs     :: [(FilePath, WatchDepth)]
  , command       :: Maybe String
  , followTail    :: Bool
  , showRunning   :: Bool
  , incremental   :: Bool
  , stabilization :: NominalDiffTime
  , debug         :: Bool
  } deriving (Show, Generic)
