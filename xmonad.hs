import XMonad
import XMonad.Layout.NoBorders
import XMonad.Util.EZConfig
import XMonad.Layout.LimitWindows
import XMonad.Hooks.ManageDocks
import XMonad.Layout.TrackFloating
import XMonad.Hooks.EwmhDesktops
import XMonad.Actions.Menu.Menus
import XMonad.Hooks.DynamicLog
import XMonad.Actions.CycleWS
import Data.Maybe
import Data.List ( (\\) )
import XMonad.Util.WorkspaceCompare ( getSortByTag )
import XMonad.Hooks.NotifyUrgencyHook
import XMonad.Actions.RotSlaves
import XMonad.Layout.AdjustableTall
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.FullscreenToggleStruts
import XMonad.Actions.SelectWindow
import XMonad.Layout.Limit2
import XMonad.Actions.WindowBringer (bringWindow)
import Control.Monad ( join )
import XMonad.Actions.DynamicWorkspaces
import qualified XMonad.Layout.Fullscreen as FS
import XMonad.Actions.Warp (warpToWindow)
import XMonad.Hooks.History
import XMonad.Hooks.WorkspaceHistory

import qualified Data.Map.Strict as M
import qualified XMonad.Actions.FlexibleManipulate as Flex
import qualified XMonad.StackSet as W

main = xmonad mconfig

getSortByTag' = ((.) minTLast) <$> getSortByTag
  where minTLast [] = []
        minTLast (x:xs)
          | (W.tag x) == minT = xs ++ [x]
          | otherwise = x:(minTLast xs)

addHistory c = c {logHook = (logHook c) >> historyHook >> workspaceHistoryHook }

addLog c = c
  {
    logHook = (logHook c) >> (dynamicLogString pp >>= xmonadPropLog)
  , startupHook = (startupHook c) >> spawn "pkill polybar; polybar -c ~/.xmonad/polybar-config example"
  } where
  whiten = wrap "%{F#fff}" "%{F-}"
  bold = wrap "%{T3}" "%{T-}"
  ul c = wrap ("%{u"++c++" +u}") "%{-u}"
  fg c = wrap ("%{F"++c++"}") "%{F-}"
  numberOfWindows = whiten . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset
  pp = def
       {
         ppTitle   = const ""
       , ppCurrent = bold . fg "#fff" . ul "#fff"
       , ppVisible = bold
       , ppHidden  = id
       , ppUrgent  = bold . fg "#f44"
       , ppExtras  = [Just <$> (gets numberOfWindows)]
       , ppSort = getSortByTag'
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
  addHistory $
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
          limit2 $
          (ajustableTall (1/2) 1) ||| Full

mkeys =
  [
    ( "M-w", spawn "chromium" )
  , ( "M-e", spawn "emacsclient -c -n" )
  , ( "M-j", windowMenu "M-j" )
  , ( "M-'", selectWindow >>= (flip whenJust (windows . W.focusWindow)) >> (warpToWindow (1/8) (1/8))  )
  , ( "M-x", commandMenu )
  , ( "M-;", workspaceMenu "M-;" )
  , ( "M-q", sysMenu "M-q" )
  , ( "M-/", toggleLimit2 )

  , ( "<XF86AudioRaiseVolume>", spawn "pamixer -i 10" )
  , ( "<XF86AudioLowerVolume>", spawn "pamixer -d 10" )
  , ( "<XF86AudioMute>", spawn "pamixer -t" )
  , ( "<XF86MonBrightnessUp>", spawn "xbacklight -inc 10" )
  , ( "<XF86MonBrightnessDown>", spawn "xbacklight -dec 10" )

  , ( "M-n", windows $ W.focusDown )
  , ( "M-M1-n", rotSlavesDown )
  , ( "M-M1-p", rotSlavesUp )

  , ( "M-y", sendT )
  , ( "M-u", bringT )

  , ( "M-p", windows $ W.focusUp )
  , ( "M-S-n", windows $ W.swapDown )
  , ( "M-S-p", windows $ W.swapUp )
  , ( "M-k", kill )
  , ( "M-C-n", findWorkspace getSortByTag' Next interestingWS 1 >>= (windows . W.greedyView))
  , ( "M-C-p", findWorkspace getSortByTag' Prev interestingWS 1 >>= (windows . W.greedyView))

  , ("M-s", swapNextScreen)
  , ("M-S-s", shiftNextScreen)
  , ("M-=", growTileVertically (1/8))
  , ("M--", growTileVertically (-1/8))
  ] ++
  concat [ [ ("M-" ++ show n, view n) , ("M-S-" ++ show n, shiftTo n)] | n <- [1 .. 9] ]
  where view n = withNthNEWorkspace W.greedyView (n - 1)
        shiftTo n = withNthNEWorkspace W.shift (n - 1)
        interestingWS = WSIs $ do hs <- gets (map W.tag . W.hidden . windowset)
                                  return (\w -> isJust (W.stack w) && W.tag w `elem` (hs \\ [minT]))
        sendT = do addHiddenWorkspace minT
                   windows (W.shift minT)
        masterOf tag ss = join $
          ((listToMaybe . W.integrate' . W.stack) <$>
            (listToMaybe $ filter ((== tag) . W.tag) $ W.workspaces ss))
        bringT = windows $ \ss -> fromMaybe ss $ (flip bringWindow ss) <$> (masterOf minT ss)

withNthNEWorkspace :: (String -> WindowSet -> WindowSet) -> Int -> X ()
withNthNEWorkspace job wnum = do ws <- nonEmptyNames
                                 case drop wnum ws of
                                   (w:_) -> windows $ job $ w
                                   [] -> return ()


nonEmptyNames :: X [WorkspaceId]
nonEmptyNames = do sort <- getSortByTag'
                   ws <- gets windowset
                   let spaces = (map W.workspace ((W.current ws):(W.visible ws))) ++
                                (filter (isJust . W.stack) $ W.hidden ws)
                   return $ map W.tag $ sort spaces

isFloating :: Window -> X Bool
isFloating w = gets ((M.member w) . W.floating . windowset)
