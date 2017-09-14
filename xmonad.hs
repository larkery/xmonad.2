import XMonad
import XMonad.Layout.NoBorders
import XMonad.Util.EZConfig
import XMonad.Layout.LimitWindows
import XMonad.Hooks.ManageDocks
import XMonad.Layout.TwoPane
import XMonad.Hooks.EwmhDesktops
import XMonad.Actions.Menu.Menus
import XMonad.Hooks.DynamicLog

main = xmonad mconfig

addLog c = c
  {
    logHook = (logHook c) >> (dynamicLogString pp >>= xmonadPropLog)
  } where
  pp = def
       {
         ppTitle   = const ""
       , ppCurrent = wrap "%{u#ffffff +u}" "%{-u}"
       , ppVisible = wrap "%{u#00ffff +u}" "%{-u}"
       , ppUrgent  = wrap "%{u#ff0000 +u}" "%{-u}"
       }

mconfig =
  addLog $
  ewmh $ docks $
  (def
    { terminal    = "urxvt"
    , modMask     = mod4Mask
    , borderWidth = 1
    , focusedBorderColor = "white"
    , normalBorderColor = "#555"
    , layoutHook = _layout
    } `additionalKeysP` mkeys)

_layout = avoidStruts $
          smartBorders $
          Tall 1 (1/8) (1/2) ||| TwoPane (1/8) (1/2)||| Full

mkeys =
  [
    ( "M-w", spawn "chromium" )
  , ( "M-e", spawn "emacsclient -c -n" )
  , ( "M-;", windowMenu )
  , ( "M-p", commandMenu )
  , ( "M-S-;", workspaceMenu )
  , ( "<XF86AudioRaiseVolume>", spawn "pamixer -i 10" )
  , ( "<XF86AudioLowerVolume>", spawn "pamixer -d 10" )
  , ( "<XF86AudioMute>", spawn "pamixer -t" )
  , ( "<XF86MonBrightnessUp>", spawn "xbacklight -inc 10" )
  , ( "<XF86MonBrightnessDown>", spawn "xbacklight -dec 10" )
  ]
