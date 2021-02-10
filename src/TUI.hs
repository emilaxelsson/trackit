{-# LANGUAGE TemplateHaskell #-}

module TUI
  ( TrackitEvent (..)
  , appMain
  ) where

import Control.Monad (guard)
import Control.Monad.Trans (liftIO)
import Data.Char (toLower)
import Data.Text (Text)
import qualified Data.Text as Text

import Lens.Micro.Platform ((&), (.~), (%~), makeLenses)

import Brick
  ( App(..)
  , BrickEvent(..)
  , EventM
  , Location (..)
  , Next
  , Size (..)
  , Widget (..)
  , attrMap
  , availHeight
  , availWidth
  , continue
  , customMain
  , getContext
  , getVtyHandle
  , halt
  , neverShowCursor
  , raw
  , translateBy
  )
import Brick.BChan (BChan)
import Graphics.Vty
  ( Event(..)
  , Image
  , Key(..)
  , Modifier(..)
  , (<|>)
  , black
  , defAttr
  , defaultConfig
  , displayBounds
  , imageWidth
  , mkVty
  , outputIface
  , text'
  , white
  , withBackColor
  , withForeColor
  )

import Options
import ParseANSI

-- | Create an 'Image' from a list of lines (ANSI codes supported)
--
-- Given the first two arguments `x` and `w`, `x` characters are be dropped at
-- the beginning of each line. After offsetting, each line is cropped to `w`
-- characters.
ansiImage ::
     Int -- ^ X offset
  -> Int -- ^ Available width
  -> [Text] -- ^ Lines
  -> Image
ansiImage w x = foldMap (mkLine . takeSegs w . dropSegs x . parseANSI)
  where
    mkLine ss
      | imageWidth line == 0 = text' defAttr " "
          -- Apparently, each line must have at least one character, otherwise
          -- it doesn't take up any vertical space.
      | otherwise = line
      where
        line = foldr (<|>) mempty [text' a s | Segment a s <- ss]
  -- Note that horizontal panning of lines cannot be done outside of this
  -- function. If the lines contain ANSI codes, plain dropping and taking of
  -- characters in the 'Text' representation may lead to strange results. So
  -- panning can only be done after ANSI parsing. (See comment on `AppState` on
  -- why the buffer isn't parsed immediately.)

-- | Case-insensitive key-press recognizer
keyPressed :: Char -> BrickEvent n e -> Bool
keyPressed c (VtyEvent (EvKey (KChar c') [])) = toLower c == toLower c'
keyPressed _ _ = False

data AppState = AppState
  { commandOutput  :: [Text] -- ^ Lines in reverse
  , commandRunning :: !Bool
  , bufferWidth    :: !Int -- ^ Width of the widest line in the buffer
  , bufferHeight   :: !Int -- ^ Height of buffer
  , _xOffset       :: !Int
  , _yOffset       :: !Int
  , updateCount    :: !Integer
  } deriving (Eq, Show)
  -- Note: One option would be to have `commandOutput :: [[Segments]]`; i.e.
  -- parse ANSI codes immediately when the buffer is read. This would add some
  -- type safety and would avoid having to parse the same line multiple times.
  -- However, tests show that this approach requires around 3 times more memory
  -- for large buffers.

makeLenses ''AppState

-- | Ensure that the offsets are within the available area
clampState ::
     (Int, Int) -- ^ Available width, height
  -> AppState
  -> AppState
clampState (w, h) s@AppState {..}
  | validOffset = s -- avoid allocation when nothing needs to change
  | otherwise = s
      & xOffset %~ (max 0 . min (bufferWidth - w))
      & yOffset %~ (max 0 . min (bufferHeight - h))
  where
    validOffset = and
      [ _xOffset >= 0
      , _xOffset <= bufferWidth - w
      , _yOffset >= 0
      , _yOffset <= bufferHeight - h
      ]

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
  , bufferWidth = 0
  , bufferHeight = 0
  , _xOffset = 0
  , _yOffset = 0
  , updateCount = 0
  }

bufferWidget ::
     AppState
  -> [Text] -- ^ Lines in reverse order
  -> Widget m
bufferWidget AppState {..} ls =
  Widget Greedy Greedy $ do
    cxt <- getContext
    let (w, h)        = (availWidth cxt, availHeight cxt)
        offsetFromEnd = bufferHeight - _yOffset - h
        visibleLines  = reverse $ take h $ drop offsetFromEnd ls
    render $ raw $ ansiImage w _xOffset visibleLines

drawApp :: Options -> AppState -> [Widget n]
drawApp Options {..} s@AppState {..} = concat
  [ guard debug          >> pure debugWidget
  , guard commandRunning >> pure runningWidget
  , pure $ bufferWidget s commandOutput
  ]
  where
    attr = defAttr `withForeColor` black `withBackColor` white

    runningText = "running.."
    runningWidget = Widget Fixed Fixed $ do
      cxt <- getContext
      let x = availWidth cxt - Text.length runningText
      render $ translateBy (Location (x, 0)) $ raw $ text' attr runningText
      -- I tried using `padLeft` instead, but it doesn't work because the
      -- padding overwrites any content below it. See this issue/question:
      -- <https://github.com/jtdaugherty/brick/issues/74>

    debugText = "Update count: " <> Text.pack (show updateCount)
    debugWidget = Widget Fixed Fixed $ do
      cxt <- getContext
      let x = availWidth cxt - Text.length debugText
          y = availHeight cxt - 1
      render $ translateBy (Location (x, y)) $ raw $ text' attr debugText

stepApp ::
     Options
  -> IO () -- ^ Update request
  -> AppState
  -> BrickEvent n TrackitEvent
  -> EventM n (Next AppState)
stepApp _ _ s (keyPressed 'q' -> True) = halt s
stepApp _ updReq s (keyPressed ' ' -> True) = liftIO updReq >> continue s
stepApp opts _ s ev = do
  vty <- getVtyHandle
  size <- liftIO $ displayBounds $ outputIface vty
  let s' = clampState size $ stepState opts ev size s
  continue s'

stepState ::
     Options
  -> BrickEvent n TrackitEvent
  -> (Int, Int) -- ^ Available width, height
  -> AppState
  -> AppState
stepState _ (AppEvent Running) _ s = s
  { commandOutput  = []
  , commandRunning = True
  , bufferWidth    = 0
  , bufferHeight   = 0
  }
stepState _ (AppEvent Done) _ s@AppState {..} = s
  { commandRunning = False
  , updateCount    = updateCount + 1
  }
stepState opts (AppEvent (AddLine line)) (_, h) s@AppState {..} = s
  { commandOutput  = line : commandOutput
  , bufferWidth    = bufferWidth `max` lengthSegs (parseANSI line)
  , bufferHeight   = bufferHeight + 1
  , _yOffset       = if followTail opts then bufferHeight + 1 - h else _yOffset
  }
stepState opts (AppEvent (UpdateBuffer buf)) (_, h) s@AppState {..} = s
  { commandOutput  = buf
  , bufferWidth    = maximum $ 0 : map (lengthSegs . parseANSI) buf
  , bufferHeight   = len
  , _yOffset       = if followTail opts then len - h else _yOffset
  , updateCount    = updateCount + 1
  }
  where
    len = length buf
stepState _ (VtyEvent (EvKey kc [])) (w, h) s@AppState {bufferHeight}
  | kc `elem` [KDown,  KChar 'j'] = s & yOffset %~ (+1)
  | kc `elem` [KUp,    KChar 'k'] = s & yOffset %~ subtract 1
  | kc `elem` [KLeft,  KChar 'h'] = s & xOffset %~ subtract (div w 2)
  | kc `elem` [KRight, KChar 'l'] = s & xOffset %~ (+ div w 2)
  | kc `elem` [KHome,  KChar 'g'] = s & yOffset .~ 0
  | kc `elem` [KEnd,   KChar 'G'] = s & yOffset .~ (bufferHeight - h)
  | kc == KPageUp                 = s & yOffset %~ subtract h
  | kc == KPageDown               = s & yOffset %~ (+h)
stepState _ (VtyEvent (EvKey kc [MCtrl])) _ s
  | kc == KChar 'u'               = s & yOffset %~ subtract 25
  | kc == KChar 'd'               = s & yOffset %~ (+25)
stepState _ _ _ s = s

myApp :: Options
  -> IO () -- ^ Update request
  -> App AppState TrackitEvent ()
myApp opts updReq = App
  { appDraw         = drawApp opts
  , appHandleEvent  = stepApp opts updReq
  , appStartEvent   = return
  , appAttrMap      = const $ attrMap defAttr []
  , appChooseCursor = neverShowCursor
  }

appMain ::
     Options
  -> IO () -- ^ Update request
  -> BChan TrackitEvent
  -> IO AppState
appMain opts updReq updEv = do
  vty <- mkVty defaultConfig
  customMain
    vty
    (mkVty defaultConfig)
    (Just updEv)
    (myApp opts updReq)
    initState
