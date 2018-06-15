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
import qualified Debug.Trace as D

import XMonad.Util.NamedWindows (getName)
import Data.Traversable (traverse)
import Data.Maybe
import Data.List (findIndex, intersperse, intersect)

data CString = Plain String | Styled String String String

text (Plain s) = s
text (Styled s _ _) = s

fg (Plain _) = "white"
fg (Styled _ f _) = f
bg (Plain _) c = c
bg (Styled _ _ b) _ = b

type XScreen = W.Screen WorkspaceId (Layout Window) Window ScreenId ScreenDetail

data Line = Line {_font :: String, _left :: [CString], _right :: [CString]}

data HintContent = HintContent
  {
    _bg :: String,
    _lines :: [Line]
  }

data Hint = Hint {
     _window :: [Window],
     _timer :: Maybe TimerId
} deriving (Read, Show, Typeable)

instance ExtensionClass Hint where
    initialValue = Hint [] Nothing

content :: X [WorkspaceId] -> XScreen -> X HintContent
content nonEmptyNames s = do
  hiddenTags <- map W.tag <$> gets (W.hidden . windowset)
  mainTag <- (W.tag . W.workspace) <$> gets (W.current . windowset)
  tags <- nonEmptyNames
  cal <- io $(getClockTime >>= toCalendarTime)

--  wTitle <- traverse (fmap show . getName) (fmap W.focus $ W.stack $ W.workspace s)
  
  let number n tag = (show n) ++ ":" ++ tag

      datestr = formatCalendarTime defaultTimeLocale "%a %b %d %H:%M" cal

      sTag = (W.tag $ W.workspace s)

      isCurrent = sTag == mainTag

      dot = Plain " » "

      layoutS = description $ W.layout $ W.workspace $ s
      wCount = length $ W.integrate' $ W.stack $ W.workspace s

      layout | layoutS == "Full" && wCount > 1 = Styled (layoutS ++ " [" ++ show wCount ++ "]") "white" "purple"
             | otherwise = Plain layoutS
  
      thisTagIndex = 1 + (fromJust $ findIndex (== sTag) tags)
      thisTag = Styled (number thisTagIndex sTag) "white" "red" 

      here = [thisTag, dot, layout]
      
      workspaces = intersperse (Plain " ") $
                   (flip map (filter ((`elem` hiddenTags). fst) (zip tags [1..])) $
                     \(tag, n) -> Plain $ if tag == "&" then tag else (number n tag))
                   
--      windowTitle = maybeToList $ flip fmap wTitle $ Plain
--      windowLine = [Line "xft:Sans-16" windowTitle []]

  if isCurrent
    then return $ HintContent (if isCurrent then "#003b3b" else "black") $ 
         [Line "xft:Sans-24" (here ++ dot:workspaces) [Plain datestr]]
    else return $ HintContent "black" $ [Line "xft:Sans-20" here [Plain datestr]]

startHintTimer :: X ()
startHintTimer = do
  id <- startTimer 0.2
  XS.modify $ \h -> h {_timer = Just id}
  return ()

displayContent :: (XScreen, HintContent) -> X Window
displayContent (screen, content) = withDisplay $ \dpy -> do
  let fontNames = map _font $ _lines content 

  fonts <- mapM initXMF fontNames
  extents <- mapM (flip textExtentsXMF " ") fonts
  let height = sum $ map (uncurry (+)) extents
      (Rectangle sx sy sw sh) = (screenRect $ W.screenDetail screen)
      r = (Rectangle sx ((fi sy) + (fi sh) - height) sw (fi height))

  win <- createNewWindow r Nothing (_bg content) False
  gc <- io $ createGC dpy win

  let printCString font left top cstring =
        printStringXMF dpy win font gc (fg cstring) (bg cstring (_bg content)) (fi left) (fi top) (text cstring)
  
  setOpacity win 0.8
  showWindow win

  let tops = map (uncurry (-)) $ zip (drop 1 $ scanl (+) 0 $ map (uncurry (+)) extents) (map snd extents)
  
  (flip mapM_) (zip (_lines content) (zip fonts tops)) $
    \(line, (font, top)) -> do
      leftWidths <- mapM (textWidthXMF dpy font) (map text (_left line))
      rightWidths <- mapM (textWidthXMF dpy font) (map text (_right line))
      let lefts = scanl (+) 0 leftWidths
          rights = drop 1 $ scanl (-) (fi sw) rightWidths
      (flip mapM_) (zip lefts (_left line)) $ \(left, t) -> do
        printCString font left top t
      (flip mapM_ ) (zip rights (_right line)) $ \(left, t) -> do
        printCString font left top t
        
      return ()
    
  
  io $ freeGC dpy gc
  mapM_ releaseXMF fonts

  return win
  
showHint :: (XScreen -> X HintContent) -> X ()
showHint content = do
  clearHint
  ws <- gets windowset
  
  let screens = (W.current ws):(W.visible ws)
  content <- mapM content screens
  windows <- mapM (displayContent) (zip screens content)
  
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
eventHook :: (XScreen -> X HintContent) -> Event -> X All
eventHook _ e@(KeyEvent {}) = withDisplay $ \d -> do
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

eventHook _ e@(ButtonEvent {}) = do
  let es = ev_state e
      isSuperPress = ((es .&. mod4Mask) /= 0)

  if isSuperPress then clearHint else return ()
  
  return (All True)
  
eventHook c e = do
  (Hint {_timer = mtimer}) <- XS.get
  whenJust mtimer $ \timer -> do
     handleTimer timer e (showHint c >> return Nothing)
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
