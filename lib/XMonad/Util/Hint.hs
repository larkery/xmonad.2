{-# LANGUAGE MultiWayIf #-}
module XMonad.Util.Hint where

import XMonad
import qualified XMonad.StackSet as W
import Data.Monoid
import Data.Bits ((.&.))
import qualified XMonad.Util.ExtensibleState as XS
import XMonad.Util.Timer
import XMonad.Util.XUtils
import XMonad.Util.Font
import XMonad.Hooks.FadeInactive (setOpacity)
import System.Time
import System.Locale

data Hint = Hint {
     _window :: [Window],
     _timer :: Maybe TimerId
} deriving (Read, Show, Typeable)

instance ExtensionClass Hint where
    initialValue = Hint [] Nothing

startHintTimer :: X ()
startHintTimer = do
  id <- startTimer 0.25
  XS.modify $ \h -> h {_timer = Just id}
  return ()

showScreenHint :: XMonadFont -> W.Screen WorkspaceId (Layout Window) Window ScreenId ScreenDetail -> X Window
showScreenHint font s = withDisplay $ \dpy -> do
  let (Rectangle sx sy sw sh) = (screenRect $ W.screenDetail s)
      r = (Rectangle sx ((fi sy) + (fi sh) - 200) sw 100)
  win <- createNewWindow r Nothing "black" False
  gc <- io $ createGC dpy win
  setOpacity win 0.8
  showWindow win
  printStringXMF dpy win font gc "white" "black" 10 40 (W.tag $ W.workspace s)
  cal <- io $(getClockTime >>= toCalendarTime)
  let datestr = formatCalendarTime defaultTimeLocale "%a %b %d %H:%M" cal
  printStringXMF dpy win font gc "white" "black" 10 80 datestr
  io $ freeGC dpy gc
  return win
  
showHint :: X ()
showHint = do
  clearHint
  ws <- gets windowset
  font <- initXMF "xft:Monospace-24"
  let screens = (W.current ws):(W.visible ws)
  windows <- mapM (showScreenHint font) screens
  releaseXMF font
  XS.put $ Hint windows Nothing
  return ()

clearHint :: X ()
clearHint = do
  (Hint {_window = mwindow}) <- XS.get
  mapM_ deleteWindow mwindow
  XS.put $ Hint [] Nothing
  return ()

-- this is a bit wrong, if we do a blocking thing
-- we should probably check the key state directly somehow
-- in the timer event
eventHook :: Event -> X All
eventHook e@(KeyEvent {}) = withDisplay $ \d -> do
  let et = ev_event_type e
      es = ev_state e
  keysym <- io $ keycodeToKeysym d (ev_keycode e) 0
  let isPress = et == keyPress && keysym == xK_Super_L
      isRelease = et == keyRelease && keysym == xK_Super_L
      isSuperPress = (et == keyPress || et == keyRelease) &&
        ((es .&. mod4Mask) /= 0) -- todo modifier keys shouldn't clear

  if | isPress -> startHintTimer
     | isRelease -> clearHint
     | isSuperPress -> clearHint
     | otherwise -> return ()

  return (All True)

eventHook e@(ButtonEvent {}) = do
  let es = ev_state e
      isSuperPress = ((es .&. mod4Mask) /= 0)

  if isSuperPress then clearHint else return ()
  
  return (All True)
  
eventHook e = do
  (Hint {_timer = mtimer}) <- XS.get
  whenJust mtimer $ \timer -> do
     handleTimer timer e (showHint >> return Nothing)
     return ()
       
  return (All True)

-- on a keypress event, we want to start the timer
-- on a key release event, we want to clear the hint
-- which means closing the window if it exists and clear
-- timer if it exists
-- on a timer event, we want to handle it if it is our timer

grabPress :: X ()
grabPress = do
  XConf { display = dpy, theRoot = rootw } <- ask
  sym <- io $ keysymToKeycode dpy xK_Super_L
  io $ do grabKey dpy sym noModMask rootw False grabModeAsync grabModeAsync
          grabKey dpy sym shiftMask rootw False grabModeAsync grabModeAsync
