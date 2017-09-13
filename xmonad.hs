import XMonad
import XMonad.Layout.NoBorders
import XMonad.Util.EZConfig
import XMonad.Layout.LimitWindows
import XMonad.Hooks.ManageDocks
import XMonad.Layout.TwoPane
import XMonad.Hooks.EwmhDesktops

main = xmonad _config

_config =
  ewmh $ docks $
  (def
    { terminal    = "urxvt"
    , modMask     = mod4Mask
    , borderWidth = 1
    , focusedBorderColor = "white"
    , normalBorderColor = "#555"
    , layoutHook = _layout
    } `additionalKeysP` _keys)

_layout = avoidStruts $
          smartBorders $
          Tall 1 (1/8) (1/2) ||| TwoPane (1/8) (1/2)||| Full

_keys =
  [
    ( "M-w", spawn "chromium" )
  , ( "M-e", spawn "emacsclient -c -n" )
  ]
