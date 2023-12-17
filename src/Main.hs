{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use fewer imports" #-}


module Main where

import           Brick                      hiding (on)
import           Brick.BChan                (BChan, newBChan, writeBChan)
import           Brick.Widgets.Border       (border, borderWithLabel,
                                             joinableBorder)
import           Brick.Widgets.Border.Style (unicodeRounded)
import           Control.Concurrent         (ThreadId, forkIO, killThread,
                                             threadDelay)
import           Control.Monad              (forever, when)
import           Control.Monad.Reader       (MonadIO (liftIO), unless, void)
import           Data.Bool                  (bool)
import           Data.List                  (intersperse)
import           Data.Text                  (Text, center)
import           Graphics.Vty
import           Lens.Micro.Mtl             (assign, use, view, (%=), (.=))
import           System.Exit                (exitFailure)
import           Text.Printf                (printf)
import           Text.RawString.QQ          (r)

import           Data.Functor               ((<&>))
import           Lens.Micro                 (Getting, both, over, (^.))
import           Lens.Micro.Mtl             ((+=))
import           Lens.Micro.TH              (makeLenses)

import           Data.Tuple                 (swap)
import           Foreign.C                  (CBool)
import           Game                       (Board (..), clearBoard, cullBoring,
                                             flipBoardValue, initializeBoard,
                                             randomizeBoard, renderBoard,
                                             resizeBoard, setBoardValue,
                                             stepBoard)


data TickSpeed = Slow | Normal | Fast
  deriving (Show, Enum, Ord, Eq, Bounded)

data Tick = Tick

type Status = Bool

pattern Playing, Paused :: Bool
pattern Playing = True
pattern Paused  = False
{-# COMPLETE Playing, Paused #-}


data St = MkState
  { _status     :: Status
  , _tickSpeed  :: TickSpeed
  , _board      :: Board
  , _timer      :: (BChan Tick, ThreadId)
  , _fullscreen :: Bool
  , _help       :: Bool
  , _mouseHold  :: Bool
  , _paintingT  :: CBool
  }

makeLenses ''Board
makeLenses ''St

data Name = View | Play | Step | Speed | Clear | Random | Fullscreen | Help | Quit
  deriving (Show, Ord, Eq)


emptyWhen :: St -> Getting Bool St Bool -> Bool -> Widget Name -> Widget Name
emptyWhen st getter mode widget = if view getter st == mode then emptyWidget else widget

drawUi :: St -> [Widget Name]
drawUi st =
  [ emptyWhen st help False helpLayer
  , viewLayer st <=>
    emptyWhen st fullscreen True (buttonLayer st)
  ]

buttonLayer :: St -> Widget Name
buttonLayer st = withBorderStyle unicodeRounded $
  joinBorders . border $ hBox buttons
  where
    mkButton :: Name -> Text -> Widget Name
    mkButton name label =
      clickable name $ txt . Data.Text.center 5 ' ' $ label

    playButton, speedButton :: Widget Name
    playButton = if view status st
      then modifyDefAttr (const $ fg red) $ mkButton Play "⏸"
      else modifyDefAttr (const $ fg cyan) $ mkButton Play "▶"

    speedButton = clickable Speed . padLeftRight 2 $
      hBox [speedColor s $ str "⏵" | s <- [minBound..maxBound]]
      where
      speedColor speed = if view tickSpeed st >= speed
         then modifyDefAttr (const $ fg cyan)
         else id

    joinableV = joinableBorder (pure False)
      { eBottom = True
      , eTop    = True
      }

    buttons =
      intersperse joinableV
      [ playButton
      , mkButton Step "⏭"
      , speedButton
      , mkButton Clear "■"
      , mkButton Random "Ɍ"
      , let lsteps = view (board . steps) st
            lpop   = fromIntegral @_ @Int $ view (board . population) st
        in padRight Max $ str $ printf " Steps: %d  Population: %d" lsteps lpop
      , mkButton Fullscreen "⛶"
      , mkButton Help "?"
      , mkButton Quit "🗙"
      ]

viewLayer :: St -> Widget Name
viewLayer st = viewport View Both $ txt (view (board . asText) st)

helpLayer :: Widget Name
helpLayer = Widget Fixed Fixed $ do
  c <- getContext
  let h = c^.availHeightL
      w = c^.availWidthL
      widget = borderWithLabel (txt "Keybinds") $ str msg
  render $ translateBy (Location ((w - wSize) `div` 2, (h - hSize) `div` 2 - 2)) widget
  where
    (hSize, wSize) = (length (lines msg) + 2, maximum (length <$> lines msg) + 2)
    msg = [r|
Space   -  Pause/Unpause
s       -  Step
1/2/3   -  Change speed
c       -  Clears the map
r       -  Random start
d       -  Cull some patterns
f       -  Fullscreen
h       -  Toggles this popup
|]



keyToEvent :: Key -> EventM n St ()
keyToEvent = \case
  KEsc      {- quit          -} -> halt
  KChar ' ' {- puase/unpause -} -> pauseUnpauseEvent
  KChar 's' {- step          -} -> stepBoardEvent
  KChar '1' {- speed slow    -} -> tickSpeed .= Slow >> setNewSpeedEvent
  KChar '2' {- speed normal  -} -> tickSpeed .= Normal >> setNewSpeedEvent
  KChar '3' {- speed fast    -} -> tickSpeed .= Fast >> setNewSpeedEvent
  KChar 'c' {- clear         -} -> clearBoardEvent >> pauseEvent
  KChar 'r' {- random        -} -> randomBoardEvent
  KChar 'd' {- culls some patterns -} -> cullBoringEvent
  KChar 'f' {- fullscreen    -} -> fullscreen %= not
  KChar 'h' {- help          -} -> help %= not
  _                             -> continueWithoutRedraw

mouseUpEvent :: (Name, Maybe Button, Location) -> EventM n St ()
mouseUpEvent (Play, Just BLeft, _) = pauseUnpauseEvent
mouseUpEvent (Step,   _, _) = stepBoardEvent
mouseUpEvent (Speed,  _, _) = tickSpeed %= (bool minBound . succ <*> (maxBound /=)) >> setNewSpeedEvent
mouseUpEvent (Clear,  _, _) = clearBoardEvent >> pauseEvent
mouseUpEvent (Random, _, _) = randomBoardEvent
mouseUpEvent (Fullscreen, _, _) = fullscreen %= not
mouseUpEvent (Help,   _, _) = help %= not
mouseUpEvent (Quit,   _, _) = halt
mouseUpEvent _              = continueWithoutRedraw

appEvent :: BrickEvent Name Tick -> EventM Name St ()
appEvent (MouseUp name mb loc) = (mouseHold .= False) >> mouseUpEvent (name, mb, loc)
appEvent (AppEvent Tick) = stepBoardEvent
appEvent (MouseDown View BLeft _ loc) = mouseBoardEvent loc >> mouseHold .= True
appEvent (VtyEvent (EvKey key [])) = keyToEvent key
appEvent (VtyEvent (EvResize _ _)) = resizeEvent
appEvent _ = continueWithoutRedraw

resizeEvent :: EventM Name St ()
resizeEvent = do
  isFs <- use fullscreen
  cc <- lookupViewport View
  (width, height) <- case cc of
    Nothing -> error "Not sure when this happens"
    Just vp -> return $ view vpSize vp
  boardPtr <- use $ board . ptr
  let fhight = if isFs then height else height + 3
  liftIO $ resizeBoard (fromIntegral fhight, fromIntegral width) boardPtr

setNewSpeedEvent :: EventM n St ()
setNewSpeedEvent = do
  st <- use status
  when st $ do
    nspeed <- use tickSpeed
    (chan, threadId) <- use timer
    liftIO (killThread threadId)
    liftIO (createTimer chan nspeed) >>= assign timer

pauseEvent :: EventM n St ()
pauseEvent = use timer >>= liftIO . killThread . snd >> status .= Paused

pauseUnpauseEvent :: EventM n St ()
pauseUnpauseEvent = do
  nstat  <- use status
  nspeed <- use tickSpeed
  (chan, _) <- use timer
  status %= not
  case not nstat of
    Playing -> liftIO (createTimer chan nspeed) >>= assign timer
    Paused  -> pauseEvent

redraw :: EventM n St ()
redraw = use (board . ptr) >>= liftIO . renderBoard >>= \(rtext, pop)
  -> assign (board . asText) rtext
  >> assign (board . population) pop

stepBoardEvent, clearBoardEvent, randomBoardEvent, cullBoringEvent :: EventM n St ()
stepBoardEvent = ((board . steps) += 1) >> use (board . ptr) >>= liftIO . stepBoard >> redraw
clearBoardEvent = use (board . ptr) >>= liftIO . clearBoard >> ((board . steps) .= 0) >> redraw
randomBoardEvent = use (board . ptr) >>= liftIO . randomizeBoard >> ((board . steps) .= 0) >> redraw
cullBoringEvent = use (board . ptr) >>= liftIO . cullBoring >> redraw

mouseBoardEvent :: Location -> EventM n St ()
mouseBoardEvent l = do
  boardPtr <- use (board . ptr)
  mouseHS <- use mouseHold
  if not mouseHS then do {
      newValue <- liftIO . flipBoardValue (swap $ over both fromIntegral (loc l)) $ boardPtr;
      paintingT .= newValue
    }
  else do
    curT <- use paintingT
    liftIO . setBoardValue (swap $ over both fromIntegral (loc l)) curT $ boardPtr

  redraw

app :: App St Tick Name
app = App { appDraw = drawUi
          , appStartEvent = do
              vty@Vty{outputIface = Output{displayBounds}} <- getVtyHandle
              liftIO $ setMode (outputIface vty) Mouse True
              (width, hight) <- liftIO displayBounds
              boardPtr <- use (board . ptr)
              liftIO $ resizeBoard (fromIntegral hight, fromIntegral width) boardPtr
              return ()
          , appHandleEvent = appEvent
          , appAttrMap = const $ attrMap defAttr []
          , appChooseCursor = showFirstCursor
          }

checkForMouseSupport :: IO ()
checkForMouseSupport = do
    vty <- mkVty =<< standardIOConfig
    unless (supportsMode (outputIface vty) Mouse) $ do
        putStrLn "Error: this terminal does not support mouse interaction"
        exitFailure
    shutdown vty

type MicroSeconds = Int
refreshDelay :: TickSpeed -> MicroSeconds
refreshDelay = \case
  Slow   -> 400_000 -- 400 milisecons
  Normal -> 90_000
  Fast   -> 20_000

createTimer :: BChan Tick -> TickSpeed -> IO (BChan Tick, ThreadId)
createTimer chan ts = do
  let rs = refreshDelay ts
  thread <- forkIO $ forever $ writeBChan chan Tick >> threadDelay rs
  return (chan, thread)

main :: IO ()
main = do
  checkForMouseSupport
  chan <- newBChan 10
  ntimer <- createTimer chan Normal
  
  let initialBoard =  initializeBoard (3, 5) <&> \boardPtr -> MkBoard
        { _ptr    = boardPtr
        , _asText = mempty
        , _steps  = 0
        , _population = 0
        }

  initBoard <- initialBoard

  let initialState = MkState
        { _status     = Playing
        , _tickSpeed  = Normal
        , _board      = initBoard
        , _timer      = ntimer
        , _fullscreen = False
        , _help       = False
        , _mouseHold  = False
        , _paintingT  = minBound
        }

      customMainInit = do
        let builder = mkVty defaultConfig
        initialVty <- builder
        customMain initialVty builder (Just chan) app initialState

  void customMainInit
