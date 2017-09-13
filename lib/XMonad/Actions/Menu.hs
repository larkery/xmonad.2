{-# LANGUAGE ScopedTypeVariables, MultiParamTypeClasses, Rank2Types #-}
module XMonad.Actions.Menu where

import XMonad
import XMonad.Util.Font
import XMonad.Util.Types
import XMonad.Util.Stack -- for zipper
import Control.Arrow (first, (&&&), (***))
import Control.Monad.State
import Control.Monad
import Data.Maybe

import qualified XMonad.StackSet as W

rowLimit = 30

data MenuConfig = MenuConfig
  { _foreground :: String,
    _background :: String,
    _font :: String
  }

data MenuState a b = MenuState
  { _config :: MenuConfig,
    _window :: !Window,
    _display :: !Display,
    _gc :: !GC,
    _xfont :: XMonadFont,
    _done :: Bool,

    _generator :: String -> X [a],
    _input :: Zipper Char,
    _action :: Maybe b,
    _choices :: Zipper a
  }

class Options a b where
  options :: a -> [b]

type Menu a b = StateT (MenuState a b) X

menu :: forall a b . (Show a, Show b, Options a b) =>
  MenuConfig -> (String -> X [a]) -> X (Maybe (a, b))
menu c g = do
  XConf {display = d, theRoot = rw, XMonad.config = XConfig {modMask = mod} } <- ask
  (Rectangle sx sy sw sh) <- gets $ screenRect . W.screenDetail . W.current . windowset

  font <- initXMF $ _font c
  -- determine screen coordinates and dimensions of window
  -- xmoveresizewindow
  win <- io $ mkUnmanagedWindow d (defaultScreenOfDisplay d) rw sx sy 200 1
  gc <- io $ createGC d win
  io $ mapWindow d win
  io $ selectInput d win $ exposureMask .|. keyPressMask
  input <- (take rowLimit) <$> g ""

  let state = MenuState
        { _config = c,
          _window = win,
          _display = d,
          _gc = gc,
          _xfont = font,
          _done = False,
          _generator = g,
          _input = fromIndex "" 0,
          _action = Nothing,
          _choices = fromIndex input 0
        }

  evalStateT runLoop (state :: MenuState a b)

  releaseXMF font
  io $ do freeGC d gc
          destroyWindow d win

  return Nothing

runLoop :: forall a b . (Show a, Show b) => Menu a b ()
runLoop = do
  (disp, win) <- gets (_display &&& _window)

  status <- io $ grabKeyboard disp win True grabModeAsync grabModeAsync currentTime
  when (status == grabSuccess) $ do
    -- render the window once to start with
    render
    -- start reading keys
    handleKeys
    io $ ungrabKeyboard disp currentTime

  io $ sync disp False

render :: (Show a, Show b) => Menu a b ()
render = do
  MenuState { _display = disp, _window = win,
              _gc = gc,        _xfont = font,
              _choices = choices } <- get

  let choiceNames = fst $ toIndex $ mapZ_ show choices

  choiceHeights <- mapM (textExtentsXMF font) choiceNames

  let heights = map (\(a, b) -> a + b) choiceHeights
      height = sum heights

  -- make window just big enough
  io $ resizeWindow disp win 200 (2+fi height)
  -- render all the strings

  return ()

handleKeys :: Menu a b ()
handleKeys = return ()

-- | Creates a window with the attribute override_redirect set to True.
-- Windows Managers should not touch this kind of windows.
mkUnmanagedWindow :: Display -> Screen -> Window -> Position
                  -> Position -> Dimension -> Dimension -> IO Window
mkUnmanagedWindow d s rw x y w h = do
  let visual = defaultVisualOfScreen s
      attrmask = cWOverrideRedirect
  allocaSetWindowAttributes $
         \attributes -> do
           set_override_redirect attributes True
           createWindow d rw x y w h 0 (defaultDepthOfScreen s)
                        inputOutput visual attrmask attributes
