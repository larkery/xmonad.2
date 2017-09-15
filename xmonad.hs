import XMonad
import XMonad.Layout.NoBorders
import XMonad.Util.EZConfig
import XMonad.Layout.LimitWindows
import XMonad.Hooks.ManageDocks
import XMonad.Layout.TwoPane
import XMonad.Layout.TrackFloating
import XMonad.Hooks.EwmhDesktops
import XMonad.Actions.Menu.Menus
import XMonad.Hooks.DynamicLog
import XMonad.Actions.CycleWS
import Data.Maybe
import XMonad.Util.WorkspaceCompare ( getSortByIndex )
import XMonad.Hooks.NotifyUrgencyHook
import XMonad.Actions.RotSlaves

import qualified XMonad.StackSet as W

main = xmonad mconfig

addLog c = c
  {
    logHook = (logHook c) >> (dynamicLogString pp >>= xmonadPropLog)
  , startupHook = (startupHook c) >> spawn "pkill polybar; polybar -c ~/.xmonad/polybar-config example"
  } where
  pp = def
       {
         ppTitle   = const ""
       , ppCurrent = wrap "%{u#ffffff +u F#fff}" "%{-u F-}"
       , ppVisible = wrap "%{u#00ff00 +u F#fff}" "%{-u F-}"
       , ppUrgent  = wrap "%{u#ff0000 +u}" "%{-u}"
       }

mconfig =
  notifyUrgent $
  addLog $
  ewmh $ docks $
  (def
    { terminal    = "urxvt"
    , modMask     = mod4Mask
    , borderWidth = 1
    , focusedBorderColor = "white"
    , normalBorderColor = "#555"
    , layoutHook = _layout
    , workspaces = ["main", "other"]
    } `additionalKeysP` mkeys)

_layout = trackFloating $
          avoidStruts $
          smartBorders $
          Tall 1 (1/8) (1/2) ||| TwoPane (1/8) (1/2)||| Full

mkeys =
  [
    ( "M-w", spawn "chromium" )
  , ( "M-e", spawn "emacsclient -c -n" )
  , ( "M-j", windowMenu "M-j" )
  , ( "M-x", commandMenu )
  , ( "M-;", workspaceMenu )
  , ( "M-q", sysMenu )

  , ( "<XF86AudioRaiseVolume>", spawn "pamixer -i 10" )
  , ( "<XF86AudioLowerVolume>", spawn "pamixer -d 10" )
  , ( "<XF86AudioMute>", spawn "pamixer -t" )
  , ( "<XF86MonBrightnessUp>", spawn "xbacklight -inc 10" )
  , ( "<XF86MonBrightnessDown>", spawn "xbacklight -dec 10" )

  , ( "M-n", windows $ W.focusDown )
  , ( "M-M1-n", rotSlavesDown )
  , ( "M-M1-p", rotSlavesUp )

  , ( "M-p", windows $ W.focusUp )
  , ( "M-S-n", windows $ W.swapDown )
  , ( "M-S-p", windows $ W.swapUp )
  , ( "M-k", kill )


  , ("M-s", swapNextScreen)
  , ("M-S-s", shiftNextScreen)
  ] ++
  concat [ [ ("M-" ++ show n, view n) , ("M-S-" ++ show n, shiftTo n)] | n <- [1 .. 9] ]
  where view n = withNthNEWorkspace W.greedyView (n - 1)
        shiftTo n = withNthNEWorkspace W.shift (n - 1)

withNthNEWorkspace :: (String -> WindowSet -> WindowSet) -> Int -> X ()
withNthNEWorkspace job wnum = do ws <- nonEmptyNames
                                 case drop wnum ws of
                                   (w:_) -> windows $ job $ w
                                   [] -> return ()


nonEmptyNames :: X [WorkspaceId]
nonEmptyNames = do sort <- getSortByIndex
                   ws <- gets windowset
                   let spaces = (map W.workspace ((W.current ws):(W.visible ws))) ++
                                (filter (isJust . W.stack) $ W.hidden ws)
                   return $ map W.tag $ sort spaces
