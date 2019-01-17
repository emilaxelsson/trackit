module Main (main) where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.STM.TVar (TVar, newTVarIO, readTVar, writeTVar)
import Control.Monad (fail, forever, guard, void, when)
import Control.Monad.STM (atomically)
import Control.Monad.Trans (liftIO)
import Data.Char (toLower)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Version (showVersion)
import Data.Time.Clock (NominalDiffTime, diffUTCTime, getCurrentTime)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LText
import GHC.Generics (Generic)
import System.Exit (exitSuccess)
import System.IO (BufferMode (..))
import qualified System.Process.ListLike as Process
import qualified System.Process.Text as Text

import System.FSNotify (eventTime, withManager)
import qualified System.FSNotify as FSNotify

import Options.Generic
       (ParseRecord (..), type (<?>) (..), getWithHelp, lispCaseModifiers,
        parseRecordWithModifiers, shortNameModifier)

import Brick
import Brick.BChan
import Graphics.Vty

import qualified Paths_trackit as Trackit
import ParseANSI

type LText = LText.Text

data CmdOptions = CmdOptions
  { _watchDir      :: Maybe FilePath <?> "Directory to watch for changes in (not sub-directories). Cannot be used together with '--watch-tree'."
  , _watchTree     :: Maybe FilePath <?> "Directory tree to watch for changes in (including sub-directories). Cannot be used together with '--watch-dir'."
  , _command       :: Maybe String   <?> "Command to run"
  , _maxLines      :: Maybe Int      <?> "Maximum number of lines to show (default: 400)"
  , _followTail    :: Bool           <?> "Follow the tail of the generated output."
  , _showRunning   :: Bool           <?> "Display a message while the command is running."
  , _incremental   :: Bool           <?> "Allow output to be updated incrementally. Redraws the buffer for every output line, so should only be used for \
                                         \slow outputs. Implies '--show-running'."
  , _stabilization :: Maybe Int      <?> "Minimal time (milliseconds) between any file event and the next command update (default: 200)"
  , _version       :: Bool           <?> "Print the version number"
  , _help          :: Bool
  , _debug         :: Bool           <?> "Show debug information in the lower right corner"
  } deriving (Show, Generic)

-- `--show-running` is not on by default because it causes a quick flickering
-- for fast commands.

shortName :: String -> Maybe Char
shortName "_watchDir" = Just 'd'
shortName "_watchTree" = Just 't'
shortName "_showRunning" = Just 'r'
shortName "_debug" = Just 'g'
shortName (_:c:_) = Just c
shortName _ = Nothing

instance ParseRecord CmdOptions where
  parseRecord =
    parseRecordWithModifiers
      lispCaseModifiers {shortNameModifier = shortName}

data WatchDepth
  = Single
  | Recursive
  deriving (Eq, Show)

data Options = Options
  { watchDir      :: Maybe (FilePath, WatchDepth)
  , command       :: Maybe String
  , maxLines      :: Int
  , followTail    :: Bool
  , showRunning   :: Bool
  , incremental   :: Bool
  , stabilization :: NominalDiffTime
  , debug         :: Bool
  } deriving (Show, Generic)

watchDirError =
  "The flags '--watch-dir' and '--watch-tree' cannot be used together."

getOptions :: IO Options
getOptions = do
  (CmdOptions {..}, showHelp) <- getWithHelp "trackit"
  if | _help -> showHelp >> exitSuccess
     | unHelpful _version ->
       do putStrLn $ showVersion Trackit.version
          exitSuccess
     | otherwise ->
       do watchDir <- case (unHelpful _watchDir, unHelpful _watchTree) of
            (Nothing, Nothing) -> return Nothing
            (Just d, Nothing) -> return $ Just (d, Single)
            (Nothing, Just t) -> return $ Just (t, Recursive)
            _ -> fail watchDirError
          let command       = unHelpful _command
              maxLines      = fromMaybe 400 $ unHelpful _maxLines
              followTail    = unHelpful _followTail
              showRunning   = unHelpful _showRunning
              incremental   = unHelpful _incremental
              stabPerMs     = fromMaybe 200 $ unHelpful _stabilization
              stabilization = fromIntegral stabPerMs / 1000
              debug         = unHelpful _debug
          return $ Options {..}

ansiImage :: Text -> Image
ansiImage = foldMap mkLine . map parseANSI . Text.lines
  where
    mkLine ss =
      foldr (<|>) mempty [text' a s | Segment a s <- ss]

-- | Make a 'Context'-aware 'Widget' with 'Fixed' size
withContext :: (Context -> Widget n) -> Widget n
withContext k = Widget Fixed Fixed $ do
  cxt <- getContext
  render (k cxt)

-- | Create a bold line looking like this:
--
-- > ---------------- <info> ----------------
infoLine :: Text -> Text
infoLine info = Text.concat
  [ "\ESC[1m"
  , line1
  , " "
  , info
  , " "
  , line2
  , "\ESC[m"
  ]
  where
    l = 38 - Text.length info
    line1 = Text.replicate (l     `div` 2) "-"
    line2 = Text.replicate ((l+1) `div` 2) "-"


-- | Limit the text to @n@ lines (because large buffers make the app slow)
limit :: Int -> [Text] -> [Text]
limit n ls = ls' ++ case rest of
  [] -> [eof]
  _ -> [pruningNotification]
  where
    (ls', rest) = splitAt n ls
    eof = infoLine "End of output"
    pruningNotification =
      infoLine $ Text.unwords ["Lines beyond", Text.pack (show n), "pruned"]

helpText :: [Text]
helpText =
  [ "No command provided. Run 'trackit --help' for help.\n\n"
  , "Press 'q' to exit this window."
  ]

concatChunks :: [Process.Chunk LText] -> LText
concatChunks cs = LText.concat [c | Process.Stdout c <- cs]
  -- I initially thought that there would be one chunk per line given the
  -- `LineBuffering` setting, but this doesn't seem to be the case. Hence
  -- `Text.lines` is needed on the result.
  --
  -- There is an instance of `readCreateProcessLazy` that can return
  -- `(ExitCode, LText, LText)` directly without any chunks. However, that
  -- instance doesn't seem to have the desired laziness.

-- | Run the command provided by the user, or print a helpful text if no command
-- was given
runCMD :: Options -> IO [Text]
runCMD Options {..} = case command of
  Nothing -> return helpText
  Just cmd -> do
    (_, o, _) <- Text.readCreateProcessWithExitCode (Process.shell cmd) ""
    return $ limit maxLines $ Text.lines o

-- | Run the command provided by the user, or print a helpful text if no command
-- was given
--
-- The output is returned as a lazy list of lines.
runLazyCMD :: Options -> IO [Text]
runLazyCMD Options {..} = case command of
  Nothing -> return helpText
  Just cmd ->
    limit maxLines . map LText.toStrict . LText.lines . concatChunks <$>
    Process.readCreateProcessLazy
      (Process.shell cmd, LineBuffering, LineBuffering)
      ""

-- | Case-insensitive key-press recognizer
keyPressed :: Char -> BrickEvent n e -> Bool
keyPressed c (VtyEvent (EvKey (KChar c') [])) = toLower c == toLower c'
keyPressed _ _ = False

data AppState = AppState
  { commandOutput :: [Text] -- ^ Lines in reverse
  , commandRunning :: Bool
  , updateCount :: Integer
  } deriving (Eq, Show)

-- | Explicit request to run the command and update output
data UpdateRequest = UpdateRequest
  deriving (Eq, Show)

data TrackitEvent
  = Running             -- ^ An incremental command started running
  | Done                -- ^ An incremental command is done
  | AddLine Text        -- ^ An incremental command produced a line
  | UpdateBuffer [Text] -- ^ A non-incremental command finished with the given output
  deriving (Eq, Show)

initState :: AppState
initState = AppState
  { commandOutput = []
  , commandRunning = False
  , updateCount = 0
  }

data View = TheView
  deriving (Eq, Ord, Show)

theView :: ViewportScroll View
theView = viewportScroll TheView

drawApp :: Options -> AppState -> [Widget View]
drawApp Options {..} AppState {..} = concat
  [ guard debug >> pure debugWidget
  , pure $
    viewport TheView Both $
    raw $ ansiImage $ Text.unlines $ reverse (runningInfo ++ commandOutput)
  ]
  where
    attr = defAttr `withForeColor` black `withBackColor` white

    runningInfo = if commandRunning then [infoLine "Command is running"] else []

    debugText = "Update count: " <> Text.pack (show updateCount)
    debugWidget =
      withContext $ \cxt ->
        translateBy
          (Location
             (availWidth cxt - Text.length debugText, availHeight cxt - 1)) $
        raw $ text' attr debugText

withSize :: ((Int, Int) -> EventM View ()) -> EventM View ()
withSize k = mapM_ k . fmap extentSize =<< lookupExtent TheView

stepApp ::
     Options
  -> TVar (Maybe UpdateRequest)
  -> AppState
  -> BrickEvent View TrackitEvent
  -> EventM View (Next AppState)
stepApp _ _ s (keyPressed 'q' -> True)        = halt s
stepApp _ _ s (VtyEvent (EvKey KDown []))     = theView `vScrollBy` 1 >> continue s
stepApp _ _ s (VtyEvent (EvKey KUp []))       = theView `vScrollBy` (-1) >> continue s
stepApp _ _ s (VtyEvent (EvKey KLeft []))     = withSize (\(w, _) -> theView `hScrollBy` (negate $ div w 2)) >> continue s
stepApp _ _ s (VtyEvent (EvKey KRight []))    = withSize (\(w, _) -> theView `hScrollBy` (div w 2)) >> continue s
stepApp _ _ s (VtyEvent (EvKey KHome _))      = vScrollToBeginning theView >> continue s
stepApp _ _ s (VtyEvent (EvKey KEnd _))       = vScrollToEnd theView >> continue s
stepApp _ _ s (VtyEvent (EvKey KPageUp []))   = withSize (\(_, h) -> theView `vScrollBy` (negate h)) >> continue s
stepApp _ _ s (VtyEvent (EvKey KPageDown [])) = withSize (\(_, h) -> theView `vScrollBy` h) >> continue s
stepApp _ updReq s (VtyEvent (EvKey (KChar ' ') _)) = do
  liftIO $ atomically $ writeTVar updReq (Just UpdateRequest)
  continue s
stepApp _ _ s (AppEvent Running) =
  continue s
    { commandOutput = []
    , commandRunning = True
    }
stepApp opts _ s (AppEvent Done) = do
  when (followTail opts) $ vScrollToEnd theView
  continue s
    { commandRunning = False
    , updateCount = updateCount s + 1
    }
stepApp opts _ s (AppEvent (AddLine line)) = do
  when (followTail opts) $ vScrollToEnd theView
  continue s
    { commandOutput = line : commandOutput s
    }
stepApp opts _ s (AppEvent (UpdateBuffer buf)) = do
  when (followTail opts) $ vScrollToEnd theView
  continue s
    { commandOutput = buf
    , commandRunning = False
    , updateCount = updateCount s + 1
    }
stepApp _ _ s _ = continue s

myApp :: Options -> TVar (Maybe UpdateRequest) -> App AppState TrackitEvent View
myApp opts updReq =
  App
  { appDraw = drawApp opts
  , appHandleEvent = stepApp opts updReq
  , appStartEvent = return
  , appAttrMap = const $ attrMap defAttr []
  , appChooseCursor = neverShowCursor
  }

appMain ::
     Options -> TVar (Maybe UpdateRequest) -> BChan TrackitEvent -> IO AppState
appMain opts updReq updEv =
  customMain (mkVty defaultConfig) (Just updEv) (myApp opts updReq) initState

-- | A loop that continuously looks for events in the two variables and runs the
-- given action in response. The variables are emptied whenever the action runs.
--
-- Events in the second variable immediately trigger the action. For events in
-- the second variable, the action only happens if the event occurred more than
-- 'stabilization' seconds ago. If the event occurred less time ago, it will
-- remain in the variable and processed in a later iteration (unless emptied by
-- another event in the meantime).
worker ::
     Options
  -> TVar (Maybe FSNotify.Event)
       -- ^ Variable holding the last file event that has not yet been processed
  -> TVar (Maybe UpdateRequest)
       -- ^ Variable holding a potential update request
  -> IO () -- ^ Action to perform when the file event has stabilized
  -> IO ()
worker Options {..} lastFSEv updReq action =
  forever $ do
    threadDelay loopPeriod
    t <- getCurrentTime
    act <-
      atomically $ do
        mupd <- readTVar updReq
        case mupd of
          Just UpdateRequest -> resetEvents >> return True
          Nothing -> do
            mfsEv <- readTVar lastFSEv
            case mfsEv of
              Nothing -> return False
              Just fsEv -> do
                let stable = diffUTCTime t (eventTime fsEv) >= stabilization
                when stable resetEvents
                return stable
    when act action
  where
    resetEvents = do
      writeTVar lastFSEv Nothing
      writeTVar updReq Nothing
    loopPeriod = max 10000 $ round (stabilization * 1e6 / 5)
      -- Cap at 10 ms to avoid making the loop too busy when the stabilization
      -- period is small.

-- | Run the command and feed the output lines to the GUI
updater :: Options -> BChan TrackitEvent -> IO ()
updater opts@Options {..} updEv
  | incremental = do
      writeBChan updEv Running
      ls <- runLazyCMD opts
      mapM_ (writeBChan updEv . AddLine) ls
      writeBChan updEv Done
  | otherwise = do
      when showRunning $ writeBChan updEv Running
      ls <- runCMD opts
      writeBChan updEv $ UpdateBuffer $ reverse ls

main = do
  opts@Options {..} <- getOptions
  -- Channel holding the last file event
  lastFSEv <- newTVarIO Nothing
  -- Channel holding user update requests (set to request an initial update)
  updReq   <- newTVarIO (Just UpdateRequest)
  -- Channel for GUI update events
  updEv    <- newBChan 1
  let setFsEvent ev = atomically $ writeTVar lastFSEv $ Just ev
  tid <- forkIO $ worker opts lastFSEv updReq (updater opts updEv)
  void $ case watchDir of
    Nothing -> appMain opts updReq updEv
    Just (path, depth) ->
      withManager $ \m -> do
        void $ case depth of
          Single -> FSNotify.watchDir m path (const True) setFsEvent
          Recursive -> FSNotify.watchTree m path (const True) setFsEvent
        appMain opts updReq updEv
  killThread tid

-- Note: The "debouncing" option of fsnotify makes it so that only the *first*
-- in a tight series of events is reported. However, this is problematic since
-- it means that the GUI may miss file events. This can happen if a Git command
-- performs multiple file system operations (which is usually the case) and the
-- command take more time than updating the GUI (e.g. due to the repository
-- being large). It can of course also happen if a Git command is issued just
-- after another one.
--
-- In contrast, the approach taken here is to react to the *last* in a tight
-- sequence of events. A tight sequence is defined as a sequence in which each
-- consecutive pair of events has a time distance of less than `stabilization`
-- seconds. And since `worker` runs continuously, there's never a risk that an
-- event will be missed.
