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
import Control.Monad (when)
import Graphics.X11.Xlib.Window (moveResizeWindow)
import XMonad.Util.Loggers (logCmd)

import XMonad.Hooks.UrgencyHook (readUrgents)
import XMonad.Util.NamedWindows (getName)
import Data.Traversable (traverse)
import Data.Maybe
import Data.List (findIndex, intersperse, intersect)

data CString = Plain String | Styled String String String

text (Plain s) = s
text (Styled _ _ s) = s

fg (Plain _) f = f
fg (Styled f _ _) f0 = if null f then f0 else f
bg (Plain _) c = c
bg (Styled _ b _) b0 = if null b then b0 else b

type XScreen = W.Screen WorkspaceId (Layout Window) Window ScreenId ScreenDetail

data Line = Line {_font :: String, _left :: [CString], _right :: [CString]}

data HintContent = HintContent
  {
    _bg :: String,
    _fg :: String,
    _lines :: [Line]
  }

data Hint = Hint {
     _window :: [(ScreenId, Window)],
     _timer :: Maybe TimerId
} deriving (Read, Show, Typeable)

instance ExtensionClass Hint where
    initialValue = Hint [] Nothing

clock :: X CString
clock = do
  cal <- io $(getClockTime >>= toCalendarTime)
  return $ Plain $ formatCalendarTime defaultTimeLocale "%a %b %d %H:%M" cal

wlan :: X CString
wlan = do
  mwi <- logCmd "iwgetid -r"
  return $ Plain $ fromMaybe "" mwi

mute :: X CString
mute = do
  mmu <- maybe False (== "true") <$> logCmd "pamixer --get-mute"
  return $ Plain $ if mmu then "mute" else ""
    
battery :: X CString
battery = do
  mbat <- logCmd "~/.xmonad/statusbar battery"
  let h:_ = words $ fromMaybe "" mbat
      hp :: Int
      hp = read h
      colr | hp < 10 = "red"
           | hp < 15 = "orangered"
           | hp < 30 = "orange"
           | hp < 50 = "yellow"
           | hp < 75 = "chartreuse"
           | otherwise = "green"
  return $ Styled colr "" $ "⚡" ++ fromMaybe "" mbat

content :: X [WorkspaceId] -> XScreen -> X HintContent
content nonEmptyNames s = do
  hiddenTags <- map W.tag <$> gets (W.hidden . windowset)
  mainTag <- (W.tag . W.workspace) <$> gets (W.current . windowset)
  tags <- nonEmptyNames
  time <- clock
  batt <- battery
  wi <- wlan
  mu <- mute

  urgents <- readUrgents
  wTitle <- traverse (fmap show . getName) (if null urgents
                                             then (fmap W.focus $ W.stack $ W.workspace s)
                                             else Just $ head urgents)
  
  let line n l r = Line ("xft:Sans-" ++ show n) l r
  
      number n tag = (show n) ++ ":" ++ tag
      sTag = (W.tag $ W.workspace s)

      isCurrent = sTag == mainTag

      dot = " · "

      layoutS = description $ W.layout $ W.workspace $ s
      wCount = length $ W.integrate' $ W.stack $ W.workspace s

      layout | layoutS == "Full" && wCount > 1 = Styled "orange" "" (layoutS ++ " - " ++ show wCount)
             | otherwise = Plain layoutS
  
      thisTagIndex = 1 + (fromJust $ findIndex (== sTag) tags)
      thisTag = Styled "cyan" "" (number thisTagIndex sTag)

      here = bits dot [thisTag, layout]

      bits sep xs = intersperse (Plain sep) $ filter ((/= "") . text) xs
      
      workspaces = bits " " $
                   (flip map (filter ((`elem` hiddenTags). fst) (zip tags [1..])) $
                     \(tag, n) -> Plain $ if tag == "&" then tag else (number n tag))
                   
      windowTitle
        | null urgents = map Plain $ maybeToList wTitle
        | otherwise = map (Styled "white" "red") $ maybeToList wTitle

  if isCurrent
    then return $ HintContent "black" "white" $
         [line 24 (if null workspaces then here else (here ++ (Plain dot):workspaces)) [time],
          line 16 windowTitle ((bits dot [batt, mu, wi]) ++ [Plain "  "])]
    else return $ HintContent "grey25" "white" $ [line 20 here [time]]

startHintTimer :: X ()
startHintTimer = do
  id <- startTimer 0.2
  XS.modify $ \h -> h {_timer = Just id}
  return ()

createScreenWindow :: XScreen -> X Window
createScreenWindow screen = do
  let height = 1
      (Rectangle sx sy sw sh) = (screenRect $ W.screenDetail screen)
  XConf {display = dpy, theRoot = rw} <- ask
  win <- createNewWindow (Rectangle sx (fi sy + fi sh - 1) sw 1) Nothing "black" False
  
  setOpacity win 0.8
  showWindow win
  return win

displayContent :: (XScreen, HintContent) -> Window -> X ()
displayContent (screen, content) win = withDisplay $ \dpy -> do
  let fontNames = map _font $ _lines content
      fgc = _fg content
      bgc = _bg content

  fonts <- mapM initXMF fontNames
  extents <- mapM (flip textExtentsXMF " ") fonts
  let height = 2 + (fi $ sum $ map (uncurry (+)) extents)
      (Rectangle sx sy sw sh) = screenRect $ W.screenDetail screen
  
  io $ moveResizeWindow dpy win sx sy sw (fi height)

  pixmap <- io $ createPixmap dpy win sw (fi height) (defaultDepthOfScreen (defaultScreenOfDisplay dpy))
  -- clear rectangle
  gc <- io $ createGC dpy win

  io $ do pix <- initColor dpy bgc
          setForeground dpy gc (fromJust pix)
          fillRectangle dpy pixmap gc 0 0 sw (fi height)

  let printCString font left top cstring =
        printStringXMF dpy pixmap font gc (fg cstring fgc) (bg cstring bgc) (fi left) (fi top) (text cstring)

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

  io $ do pix <- initColor dpy fgc
          setForeground dpy gc (fromJust pix)
          drawLine dpy pixmap gc 0 (fi height - 1) (fi sw) (fi height - 1)

  io $ do copyArea dpy pixmap win gc 0 0 sw (fi height) 0 0
          freeGC dpy gc
          freePixmap dpy pixmap
          
  mapM_ releaseXMF fonts

showHint :: (XScreen -> X HintContent) -> X ()
showHint content = do
  ws <- gets windowset
  (Hint {_window = windows}) <- XS.get
  
  let screens = (W.current ws):(W.visible ws)
      mWindows = map (\s -> (s, lookup (W.screen s) windows)) screens

  nWindows <- flip mapM mWindows $ \(s, mwin) ->
    case mwin of
      Just w -> return $ (W.screen s, w)
      Nothing -> do w <- createScreenWindow s
                    return $ (W.screen s, w)
  
  XS.put $ Hint nWindows Nothing
  
  content <- mapM content screens
  flip mapM_ (zip content (zip nWindows screens)) $ \(c, ((sid, win), scr)) -> displayContent (scr, c) win
  
clearHint :: X ()
clearHint = do
  (Hint {_window = mwindow}) <- XS.get
  mapM_ deleteWindow (map snd mwindow)
  XS.put $ Hint [] Nothing
  grabPress

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

  if | isPress ->  startHintTimer
     | isRelease ->  clearHint
     | isSuperPress ->  clearHint
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

  when (ev_event_type e == expose) $ do
    (Hint {_window = w}) <- XS.get
    when (not $ null w) $ showHint c
       
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
