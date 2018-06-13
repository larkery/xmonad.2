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

data Hint = Hint {
     _window :: [Window],
     _timer :: Maybe TimerId
} deriving (Read, Show, Typeable)

instance ExtensionClass Hint where
    initialValue = Hint [] Nothing

startHintTimer :: X ()
startHintTimer = do
  id <- startTimer 0.4
  XS.modify $ \h -> h {_timer = Just id}
  return ()

showScreenHint :: XMonadFont -> W.Screen WorkspaceId (Layout Window) Window ScreenId ScreenDetail -> X Window
showScreenHint font s = withDisplay $ \dpy -> do
  let (Rectangle sx sy sw sh) = (screenRect $ W.screenDetail s)
      r = (Rectangle ((fi sx) + (fi sw) `div` 4) ((fi sy) + (fi sh) `div` 4) ((fi sw) `div` 2) ((fi sh) `div` 2))
  win <- createNewWindow r Nothing "black" False
  gc <- io $ createGC dpy win
  setOpacity win 0.6
  showWindow win
  printStringXMF dpy win font gc "black" "white" 10 40 (W.tag $ W.workspace s)
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
        ((es .&. mod4Mask) /= 0)

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
