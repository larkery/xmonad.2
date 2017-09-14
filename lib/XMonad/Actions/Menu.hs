{-# LANGUAGE ScopedTypeVariables, MultiParamTypeClasses, Rank2Types #-}
module XMonad.Actions.Menu where

import XMonad
import XMonad.Util.Font
import XMonad.Util.Types
import XMonad.Util.Stack -- for zipper
import XMonad.Util.XUtils (paintWindow)
import Control.Arrow (first, (&&&), (***))
import Control.Monad.State
import Control.Monad
import Data.Maybe
import Data.List (foldl')
import Text.ParserCombinators.ReadP hiding (get)
import XMonad.Util.EZConfig (parseKey)

import qualified XMonad.StackSet as W
import qualified Data.Map.Strict as M

import qualified Debug.Trace as D

data MenuConfig a b = MenuConfig
  { _foreground :: String,
    _background :: String,
    _font :: String,
    _keymap :: [(String, Menu a b ())],
    _rowLimit :: Int,
    _location :: Rectangle -> (Dimension, Dimension) -> (Position, Position)
  }

data ExitState = Select | Cancel | Continue deriving Eq

instance (Show a, Options a b) => Default (MenuConfig a b) where
  def = MenuConfig { _foreground = "white", _background = "Black", _font = "xft:Monospace-10",
                     _keymap = [ ("C-g", quit),
                                 ("C-p", up), ("C-n", down),
                                 ("<Up>", up), ("<Down>", down),
                                 ("<Return>", select),
                                 ("<Escape>", quit),
                                 ("<Backspace>", input del),
                                 ("<Tab>", complete)
                               ],
                     _rowLimit = 25,
                     _location = middleOfScreen
                   }

middleOfScreen :: Rectangle -> (Dimension, Dimension) -> (Position, Position)
middleOfScreen (Rectangle sx sy sw sh) (width, height) =
  let hw = width `div` 2
      hh = height `div` 2
      hsw = sw `div` 2
      hsh = sh `div` 2
  in
    (fi $ (fi sx) + (hsw - hw),
     fi $ (fi sy) + (hsh - 100))

data MenuState a b = MenuState
  { _config :: MenuConfig a b,
    _window :: !Window,
    _display :: !Display,
    _gc :: !GC,
    _xfont :: XMonadFont,
    _done :: ExitState,

    _generator :: String -> X [a],
    _input :: Input,
    _action :: Maybe b,
    _choices :: Zipper a,
    _keys :: !(M.Map (KeyMask, KeySym) (Menu a b ())),
    _rect :: Rectangle
  }

data Input = Input String String deriving Eq

conc (Input a b) = a ++ b
blank (Input "" "") = True
blank _ = False
lhs (Input a _) = a

class Options a b where
  options :: a -> [b]

type Menu a b = StateT (MenuState a b) X

menu :: forall a b . (Show a, Show b, Options a b) =>
  MenuConfig a b -> (String -> X [a]) -> X (Maybe (a, b))
menu c g = do
  XConf {display = d, theRoot = rw, XMonad.config = XConfig {modMask = mod} } <- ask
  rect@(Rectangle sx sy sw sh) <- gets $ screenRect . W.screenDetail . W.current . windowset

  font <- initXMF $ _font c
  -- determine screen coordinates and dimensions of window
  -- xmoveresizewindow
  win <- io $ mkUnmanagedWindow d (defaultScreenOfDisplay d) rw (4+sx) (4+sy) 200 1
  gc <- io $ createGC d win
  io $ mapWindow d win
  io $ selectInput d win $ exposureMask .|. keyPressMask
  input <- g ""

  let state = fixAction $ MenuState
        { _config = c,
          _window = win,
          _display = d,
          _gc = gc,
          _xfont = font,
          _done = Continue,
          _generator = g,
          _input = Input "" "",
          _action = Nothing,
          _choices = fromIndex input 0,
          _keys = M.fromList $ mapMaybe (\(k, a) -> fmap (flip (,) a) (readKey mod k)) (_keymap c),
          _rect = rect
        }

  out <- evalStateT runLoop (state :: MenuState a b)

  releaseXMF font
  io $ do freeGC d gc
          destroyWindow d win

  return out

runLoop :: forall a b . (Show a, Show b, Options a b) => Menu a b (Maybe (a, b))
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

  (done, (choices, action)) <- gets (_done &&& _choices &&& _action)

  case (done, choices, action) of
    (Select, Just (W.Stack f _ _), Just ac) -> return $ Just (f, ac)
    _ -> return Nothing

-- takeZ n Nothing = (Nothing, False)
-- takeZ n (Just (W.Stack f u d)) =
--   Just $ W.Stack f (drop nu u) (take nd d)

render :: (Show a, Show b) => Menu a b ()
render = do
  MenuState { _display = disp, _window = win,
              _gc = gc,        _xfont = font,
              _choices = choices,
              _config = (MenuConfig {_foreground = fgColor, _background = bgColor, _location = loc }),
              _input = input,
              _rect = screenRectangle
              } <- get
  let choiceNamesZ = mapZ_ show choices

  choiceHeights <- mapM (textExtentsXMF font) $ fst $ toIndex $ choiceNamesZ

  let heights = map (\(a, b) -> a + b) choiceHeights
      hasInput = not $ blank input
      inputS = conc input

  inputHeight <- if hasInput then textExtentsXMF font inputS else return (0, 0)

  let height = 2+sum heights + (fst inputHeight + snd inputHeight)
      width = 400
      topRow = 1 + (fst inputHeight + snd inputHeight)

  pixmap <- io $ createPixmap disp win (fi width) (fi height) (defaultDepthOfScreen (defaultScreenOfDisplay disp))

  -- render strings from top to bottom
  let color :: String -> IO Pixel
      color x = do Just q <- io $ initColor disp x
                   return q
      paintBox x y w h c = do
        color c >>= setForeground disp gc
        fillRectangle disp pixmap gc x y w h

      printChoice y s = either (printRow fgColor bgColor y) (printRow bgColor fgColor y) s
      printRow fg bg y s = do
        (e1, e2) <- textExtentsXMF font s
        -- fill rect behind the string
        io $ paintBox 1 y (width - 2) (fi (e1+e2)) bg
        printStringXMF disp pixmap font gc fg bg 2 (y+e1) s -- for whatever reason we need a 2 here not a 1.
        -- paint bars on the sides in case it was too big
        return $ y + e1 + e2

  lift $ paintWindow pixmap width (fi height) 1 bgColor fgColor
  when hasInput $ do
    io $ paintBox 1 1 (width - 2) (fi (fst inputHeight + snd inputHeight)) bgColor
    printStringXMF disp pixmap font gc fgColor bgColor 2 (1 + fst inputHeight) ("> " ++ inputS)

  foldM_ printChoice topRow $ toTags choiceNamesZ

  let (wx, wy) = loc screenRectangle (fi width, fi height)

  io $ do color fgColor >>= setForeground disp gc
          drawLine disp pixmap gc (fi width - 1) 0 (fi width - 1) (fi height)

          moveResizeWindow disp win wx wy width (fi height)

          copyArea disp pixmap win gc 0 0 (fi width) (fi height) 0 0
          freePixmap disp pixmap

  return ()

handleKeys :: (Show a, Show b, Options a b) => Menu a b ()
handleKeys = do
  (disp, keymap) <- gets (_display &&& _keys)
  (keym, ev) <- lift $ nextKeyEvent disp
  XConf { XMonad.config = XConfig {modMask = mod} } <- lift ask

  case keym of
    Expose -> (lift $ broadcastMessage ev) >> render
    Press mask sym string -> keyAction mod keymap (mask, sym, string) >> render
    _ -> return ()

  gets ((== Continue) . _done) >>= flip when handleKeys

  where keyAction mod h (mask, sym, str) = case (M.lookup (mask, sym) h) of
          Just x -> x
          Nothing -> defaultHandle
            where
              defaultHandle
                | mask == 0 || mask == shiftMask = input (ins str)
                | otherwise = nop

-------- Menu actions for binding.
--------------------------------------------------------------------------------

fixAction :: (Options a b) => MenuState a b -> MenuState a b
fixAction s@(MenuState {_choices = Nothing}) = s {_action = Nothing}
fixAction s@(MenuState {_choices = Just (W.Stack f _ _)}) = s { _action = listToMaybe $ options f }

nop :: Menu a b ()
nop = return ()

quit :: Menu a b ()
quit = modify $ \s->s {_done = Cancel}

up :: (Options a b) => Menu a b ()
up = modify $ \s -> fixAction $ s { _choices = focusUpZ (_choices s) }

down :: (Options a b) => Menu a b ()
down = modify $ \s -> fixAction $ s { _choices = focusDownZ (_choices s) }

select :: Menu a b ()
select = modify $ \s->s{ _done = Select }

input :: (Options a b) => (Input -> Input) -> Menu a b ()
input f = do
  (curInput :: Input, generator) <- gets (_input &&& _generator)
  let input' :: Input
      input' = f curInput

  when (input' /= curInput) $ do
    choices' <- lift $ generator $ conc input'
    modify $ \s -> fixAction $ s { _choices = fromIndex choices' 0, _input = input' }

ins :: String -> Input -> Input
ins [] st = st
ins s (Input l r) = Input (l ++ s) r

del :: Input -> Input
del (Input l r) = Input (take (length l - 1) l) r

complete :: (Show a, Options a b) => Menu a b ()
complete = do
  (choices :: [String], leftIn :: String) <- gets (((map show) . fst . toIndex . _choices) &&& (lhs . _input))
  -- find common prefix of choices
  let pfx :: String
      pfx = foldl1 prefix2 (choices)
      pfx' :: String
      pfx' = (drop (length leftIn) pfx)
  input $ ins pfx'
  where
    prefix2 :: (Eq e) => [e] -> [e] -> [e]
    prefix2 _ [] = []
    prefix2 [] _ = []
    prefix2 (x:xs) (y:ys)
      | x == y    = x : prefix2 xs ys
      | otherwise = []


-------- Support functions below:
--------------------------------------------------------------------------------


-- | Creates a window with the attribute override_redirect set to True.
-- Windows Managers should not touch this kind of windows.
mkUnmanagedWindow :: Display -> Screen -> Window -> Position
                  -> Position -> Dimension -> Dimension -> IO Window
mkUnmanagedWindow d s rw x y w h = do
  let visual = defaultVisualOfScreen s
      attrmask = cWOverrideRedirect
  allocaSetWindowAttributes $
         \attributes -> do
           set_background_pixel attributes 0
           set_override_redirect attributes True
           createWindow d rw x y w h 0 (defaultDepthOfScreen s)
                        inputOutput visual attrmask attributes


data KEvent = Expose | Press KeyMask KeySym String | Release KeySym | Skip deriving (Show)

nextKeyEvent :: Display -> X (KEvent, Event)
nextKeyEvent d = do
  io $ do allocaXEvent $ \e -> do
            maskEvent d (exposureMask .|. keyPressMask .|. keyReleaseMask) e
            ev <- getEvent e
            fmap (flip (,) ev) $
              if ev_event_type ev == keyPress then
                do x <- lookupString $ asKeyEvent e
                   return $ case x of (Just ks, str) -> Press (ev_state ev) ks str
                                      _ -> Skip
              else if ev_event_type ev == keyRelease then
                     do s <- keycodeToKeysym d (ev_keycode ev) 0
                        return $ Release s
              else if ev_event_type ev == expose && ev_count ev == 0 then
                     return $ Expose
              else return Skip



readKey :: KeyMask -> String -> Maybe (KeyMask, KeySym)
readKey m = listToMaybe . parses
  where parses = map fst . filter (null.snd) . readP_to_S (parseKeyCombo m)

        indexMod = (!!) [mod1Mask,mod2Mask,mod3Mask,mod4Mask,mod5Mask]

        -- | Parse a modifier-key combination such as "M-C-s" (mod+ctrl+s).
        parseKeyCombo :: KeyMask -> ReadP (KeyMask, KeySym)
        parseKeyCombo c = do mods <- many (parseModifier c)
                             k <- parseKey
                             return (foldl' (.|.) 0 mods, k)

        -- | Parse a modifier: either M- (user-defined mod-key),
        --   C- (control), S- (shift), or M#- where # is an integer
        --   from 1 to 5 (mod1Mask through mod5Mask).
        parseModifier :: KeyMask -> ReadP KeyMask
        parseModifier c =  (string "M-" >> return c)
                           +++ (string "C-" >> return controlMask)
                           +++ (string "S-" >> return shiftMask)
                           +++ do _ <- char 'M'
                                  n <- satisfy (`elem` ['1'..'5'])
                                  _ <- char '-'
                                  return $ indexMod (read [n] - 1)
