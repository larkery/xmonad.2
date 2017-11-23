module XMonad.Actions.SelectWindow (selectWindow) where

import Graphics.X11.Xlib.Extras (getWindowAttributes,
                                 WindowAttributes (..))

import XMonad.Actions.Menu (nextKeyEvent, KEvent (..))

import XMonad.Util.Font
import XMonad.Util.XUtils
import Data.List (sortOn)
import qualified Debug.Trace as D

import XMonad
import qualified XMonad.StackSet as W
import Control.Arrow ((&&&))

selectWindow = selectWindowKeys ["asdfgzxcvb", "hjklnm,."]

selectWindowKeys :: [String] -> String -> String -> X (Maybe Window)
selectWindowKeys keyss fg bg = withDisplay $ \dpy -> do
  allWindows <- gets (map (W.integrate' . W.stack . W.workspace) . (sortOn ((rect_x &&& rect_y) . screenRect . W.screenDetail)) . (uncurry (:)) . (W.current &&& W.visible) . windowset)
  atts <- io $ mapM (mapM (getWindowAttributes dpy)) allWindows

  let windowCenter (win, (WindowAttributes {wa_x = x, wa_y = y, wa_height = h, wa_width = w})) =
        (win, (x + (w`div`2), y + (h`div`2)))

      windowVisible (_, (WindowAttributes {wa_map_state = ms})) = ms == waIsViewable

      wins = map (map windowCenter) $ map (filter windowVisible) $ map (uncurry zip) $ zip allWindows atts

  -- create a lot of little windows to select with

  font <- initXMF "xft:Sans-24"

  let winPos = map (sortOn snd) wins -- these are sorted within ws but not without!
      winKeys = concatMap (uncurry zip) $ zip keyss winPos
      pop (k, (w, (x, y))) = do
        win <- createNewWindow (Rectangle (fromIntegral x-25) (fromIntegral y-25) 50 50) Nothing bg False
        showWindow win
        paintAndWrite win font 50 50 2 bg fg fg bg [AlignCenter] [[k]]
        return $ (win, k)

      readKey = do
        (e, _) <- nextKeyEvent dpy
        case e of
          (Press mask sym (k:_)) -> return $ fst <$> lookup k winKeys
          Expose -> readKey
          _ -> readKey

  XConf { theRoot = root } <- ask
  status <- io $ grabKeyboard dpy root True grabModeAsync grabModeAsync currentTime
  selection <- if (status == grabSuccess)
    then do displays <- mapM pop winKeys
            selection <- readKey
            mapM_ (\(w, _) -> deleteWindow w) displays
            io $ ungrabKeyboard dpy currentTime
            return selection
    else return Nothing
  releaseXMF font
  return selection
