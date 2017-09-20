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
import XMonad.Layout.AdjustableTall
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.FullscreenToggleStruts
import XMonad.Actions.SelectWindow
import qualified XMonad.Layout.Fullscreen as FS

import qualified Data.Map.Strict as M
import qualified XMonad.Actions.FlexibleManipulate as Flex
import qualified XMonad.StackSet as W

main = xmonad mconfig

addLog c = c
  {
    logHook = (logHook c) >> (dynamicLogString pp >>= xmonadPropLog)
  , startupHook = (startupHook c) >> spawn "pkill polybar; polybar -c ~/.xmonad/polybar-config example"
  } where
  whiten = wrap "%{F#fff}" "%{F-}"
  bold = wrap "%{T3}" "%{T-}"
  ul c = wrap ("%{u"++c++" +u}") "%{-u}"
  fg c = wrap ("%{F"++c++"}") "%{F-}"
  pp = def
       {
         ppTitle   = const ""
       , ppCurrent = bold . fg "#fff" . ul "#fff"
       , ppVisible = bold
       , ppHidden  = id
       , ppUrgent  = bold . fg "#f44"
       , ppExtras  = [gets (Just . whiten . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset)]
       }

specialWindows c = c { manageHook = (manageHook c) <+> rules}
  where rules = composeAll [ isDialog --> doFloat
                           , transience'
                           , className =? "Xmessage" --> doFloat
                           , className =? "Yad" --> doFloat
                           , className =? "XClock" --> doFloat ]

mconfig =
  FS.fullscreenSupport $
  specialWindows $
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
    } `additionalKeysP` mkeys
    `additionalMouseBindings`
    [((mod4Mask, 1), \w -> do float <- isFloating w
                              if float
                                then (focus w >> Flex.mouseWindow Flex.discrete w)
                                else mouseResizeTile (1/4) mouseMoveWindow w
     )]
  )

_layout = trackFloating $
          fullscreenToggleStruts $
          avoidStruts $
          smartBorders $
          (ajustableTall (1/2) 1) ||| TwoPane (1/2) (1/8) ||| Full

mkeys =
  [
    ( "M-w", spawn "chromium" )
  , ( "M-e", spawn "emacsclient -c -n" )
  , ( "M-j", windowMenu "M-j" )
  , ( "M-'", selectWindow >>= flip whenJust (windows . W.focusWindow) )
  , ( "M-x", commandMenu )
  , ( "M-;", workspaceMenu "M-;" )
  , ( "M-q", sysMenu "M-q" )

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
  , ( "M-C-n", moveTo Next HiddenNonEmptyWS )
  , ( "M-C-p", moveTo Prev HiddenNonEmptyWS )

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

isFloating :: Window -> X Bool
isFloating w = gets ((M.member w) . W.floating . windowset)
