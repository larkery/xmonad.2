{-# LANGUAGE FlexibleContexts #-}
import XMonad
import XMonad.Layout.NoBorders
import XMonad.Util.EZConfig
import XMonad.Layout.LimitWindows
import XMonad.Hooks.ManageDocks
import XMonad.Layout.TrackFloating
import XMonad.Hooks.EwmhDesktops
import XMonad.Actions.Menu.Menus
import XMonad.Actions.Menu (_foreground, _background, _width)
import XMonad.Hooks.DynamicLog
import XMonad.Actions.CycleWS
import XMonad.Actions.DwmPromote
import Data.Maybe
import Data.List ( (\\), intersect, delete )
import XMonad.Util.WorkspaceCompare ( getSortByTag )
import XMonad.Hooks.NotifyUrgencyHook
import XMonad.Actions.RotSlaves
import XMonad.Layout.AdjustableTall
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.FullscreenToggleStruts
import XMonad.Actions.SelectWindow
import XMonad.Actions.WindowBringer (bringWindow)
import Control.Monad ( join, when )
import XMonad.Actions.DynamicWorkspaces
import qualified XMonad.Layout.Fullscreen as FS
import XMonad.Actions.Warp (warpToWindow)
import XMonad.Hooks.History
import XMonad.Hooks.WorkspaceHistory
import XMonad.Layout.Flip
import Graphics.X11.Xrandr (xrrSelectInput)
import Data.Monoid
import XMonad.Actions.AfterDrag (ifClick)
import XMonad.Actions.SpawnOn
import XMonad.Layout.OneBig
import XMonad.Layout.Spacing
import XMonad.Layout.SubLayouts
import XMonad.Layout.Tabbed
import XMonad.Layout.LayoutModifier (LayoutModifier, ModifiedLayout (..))
import XMonad.Layout.Simplest (Simplest (..))
import XMonad.Layout.Decoration(Decoration, DefaultShrinker)

import qualified Data.Map.Strict as M
import qualified XMonad.Actions.FlexibleManipulate as Flex
import qualified XMonad.StackSet as W
import XMonad.Layout.Renamed

import qualified Debug.Trace as D

main = xmonad mconfig

fg2 = "#00AAFF"
fg =  "#006699"  --"#8b008b"
bg = "#f6f6f6"
nborder = "#444"

cl c = c { _foreground = bg, _background = fg }

getSortByTag' = ((.) minTLast) <$> getSortByTag
  where minTLast [] = []
        minTLast (x:xs)
          | (W.tag x) == minT = xs ++ [x]
          | otherwise = x:(minTLast xs)

addHistory c = c { logHook = (logHook c) >> historyHook >> workspaceHistoryHook }

addLog c = c
  {
    logHook = (logHook c) >> (dynamicLogString pp >>= xmonadPropLog)
  , startupHook = do startupHook c
                     -- delete all empty hidden workspaces
                     windows $ \ss -> ss {W.hidden = filter (isJust . W.stack) (W.hidden ss)}
                     spawn "pkill polybar; polybar -c ~/.xmonad/polybar-config example"
                     ensureWorkspaces

  } where
  whiten = wrap "%{F#fff}" "%{F-}"
  bold = wrap "%{T3}" "%{T-}"
  ul c = wrap ("%{u"++c++" +u}") "%{-u}"
  fg c = wrap ("%{F"++c++"}") "%{F-}"
  numberOfWindows = whiten . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset
  pp = def
       {
         ppTitle   = const ""
       , ppWsSep = ", "
       , ppCurrent = bold . fg "#fff" . ul "#fff"
       , ppVisible = bold . fg "#fff"
       , ppHidden  = id
       , ppUrgent  = bold . fg "#f44"
       , ppExtras  = [Just <$> (gets numberOfWindows)]
       , ppSort = getSortByTag'
       }

specialWindows c = c { manageHook = (manageHook c) <+> manageSpawn <+> rules}
  where rules = composeAll [ isDialog --> doFloatSnap
                           , transience'
                           , className =? "Pinentry-gtk-2" --> doFloatSnap
                           , className =? "Pinentry" --> doFloatSnap
                           , className =? "Xmessage" --> doFloatSnap
                           , className =? "Yad" --> doFloatSnap
                           , className =? "XClock" --> doFloatSnap
                           , className =? "Dunst" --> doIgnore
                           ]
        doFloatSnap = doFloatDep snap
        snap (W.RationalRect x y w h) =
          W.RationalRect x' y' w h
          where
            x' | x < 0 = 0
               | x > 1 = (1 - w)
               | otherwise = x
            y' | y < 0 = 0
               | y > 1 = (1 - h)
               | otherwise = y

mconfig =
  randr $
  FS.fullscreenSupport $
  specialWindows $
  notifyUrgent $
  addLog $
  addHistory $
  ewmh $
  docks $
  (def
    { terminal    = "urxvt"
    , modMask     = mod4Mask
    , borderWidth = 2
    , focusedBorderColor = fg2
    , normalBorderColor = nborder
    , layoutHook = _layout
    , workspaces = ["main", "mail"]
    } `additionalKeysP` mkeys
    `additionalMouseBindings`
    [((mod4Mask, 1), \w -> do float <- isFloating w
                              if float
                                then (focus w >> Flex.mouseWindow Flex.discrete w >> ifClick (windows $ W.sink w))
                                else mouseResizeTile (1/4) mouseMoveWindow w
     )]
  )

_layout = trackFloating $
          fullscreenToggleStruts $
          avoidStruts $
          smartBorders $
          renamed [CutWordsLeft 1] $
          subTabbed' $
          tall ||| Full ||| big
  where tall = flipLayout $
               nm "Tall" $
               smartSpacing 3 $
               ajustableTall (1/2) 1
        nm n = renamed [Replace n]
        big = (nm "Big" $ smartSpacing 3 $ (OneBig (3/4) (3/4)))

subTabbed' :: (Eq a, LayoutModifier (Sublayout Simplest) a, LayoutClass l a) =>
              l a -> ModifiedLayout (Decoration TabbedDecoration DefaultShrinker) (ModifiedLayout (Sublayout Simplest) l) a
subTabbed' x = addTabs shrinkText thm $ subLayout [] Simplest x
  where thm = def { fontName = "xft:Sans-9"
                  , decoHeight = 16

                  , inactiveColor = nborder
                  , inactiveBorderColor = nborder
                  , inactiveTextColor = bg

                  , activeBorderColor = fg2
                  , activeColor = fg2
                  , activeTextColor = bg

                  , unmappedBorderColor = "#777"
                  , unmappedTextColor = "#eee"
                  , unmappedColor = "#777"
                  }

sysMenu k = actionMenu (cl def) k commands where
  commands = [("reload", spawn reloadCommand),
               ("hibernate", spawn "systemctl hibernate"),
               ("suspend", spawn "systemctl suspend"),
               ("wifi", spawnHere "wpa_gui"),
               ("pass", passMenu (cl def {_width = 500})),
               ("screens", connectScreens)
             ]
  reloadCommand = "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi"

mkeys =
  [
    ( "M-w", spawnHere "firefox" )
  , ( "M-S-w", spawnHere "chromium" )
  , ( "M-e", spawnHere "emacsclient -c -n" )
  , ( "M-j", windowMenu (cl $ def {_width = 512}) "M-j" )
  , ( "M-<Space>", (selectWindowColors bg fg) >>= (flip whenJust (windows . W.focusWindow)) >> warp  )
  , ( "M-f", sendMessage NextLayout )
  , ( "M-b", sendMessage ToggleStruts )
  , ( "M-x", commandMenu (cl def) )
  , ( "M-o", workspaceMenu (cl def) "M-o" )
  , ( "M-q", sysMenu "M-q" )

  , ( "M-r", toggleFlip )
  , ( "M-z", sendT )
  , ( "M-y", bringT )
  , ( "M-<Return>", withMaster (const $ windows W.shiftMaster) (windows . W.focusWindow) >> warp )

  , ( "<XF86AudioRaiseVolume>", spawn "pamixer -i 10" )
  , ( "<XF86AudioLowerVolume>", spawn "pamixer -d 10" )
  , ( "<XF86AudioMute>", spawn "pamixer -t" )
  , ( "<XF86MonBrightnessUp>", spawn "xbacklight -inc 10" )
  , ( "<XF86MonBrightnessDown>", spawn "xbacklight -dec 10" )

  , ( "M-d", nextScreen >> warp )
  , ( "M-n", windows $ W.focusDown )
  , ( "M-M1-n", withFocused (sendMessage . mergeDir id ) )
  , ( "M-M1-p", withFocused (sendMessage . mergeDir W.focusUp') )
  , ( "M-/", withFocused (sendMessage . UnMerge) )
  , ( "M-'", sendMessage ResetTiles)

  , ( "M-m", withMaster (windows . W.focusWindow) (windows . W.focusWindow) >> warp )

  , ( "M-p", windows $ W.focusUp )
  , ( "M-S-n", windows $ W.swapDown )
  , ( "M-S-p", windows $ W.swapUp )
  , ( "M-k", kill )
  , ( "M-C-n", findWorkspace getSortByTag' Next interestingWS 1 >>= (windows . W.greedyView))
  , ( "M-C-p", findWorkspace getSortByTag' Prev interestingWS 1 >>= (windows . W.greedyView))

  , ("M-s", swapNextScreen)
  , ("M-S-s", shiftNextScreen)
  , ("M-=", growTileVertically (1/4))
  , ("M--", growTileVertically (-1/4))

  , ("M-S-/", spawn "notify-send \"Check mail\"; VERBOSE=1 notmuch new")
  ] ++
  concat [ [ ("M-" ++ show n, view n) , ("M-S-" ++ show n, shiftTo n)] | n <- [1 .. 9] ]
  where withMaster a b = do
          st <- gets (W.stack . W.workspace . W.current . windowset)
          lwh <- orderedWindows
          let here = intersect lwh $ W.integrate' st
              after w = listToMaybe $ delete w here
          case st of
            (Just (W.Stack f [] d)) ->
              case (after f, d) of
                (Just w2, _) -> b w2
                (_, x:xs) -> b x
                _ -> b f
            (Just (W.Stack _ xs _)) -> a (last xs)
            _ -> return ()

        view n = withNthNEWorkspace W.greedyView (n - 1)
        shiftTo n = withNthNEWorkspace W.shift (n - 1)
        warp = warpToWindow (1/2) (1/2)
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

updateScreens :: X ()
updateScreens = spawn "autorandr --skip-options gamma --change"

connectScreens :: X ()
connectScreens = spawn "autorandr --skip-options gamma --change --default horizontal"

selectRandrEvents :: X ()
selectRandrEvents = do
  dpy <- asks display
  root <- asks theRoot
  io $ xrrSelectInput dpy root rrScreenChangeNotifyMask

onOutputChanged :: X () -> Event -> X All
onOutputChanged a e@(RRScreenChangeNotifyEvent {}) = D.traceShow e $ a >> return (All True)
onOutputChanged _ _ = return (All True)

ensureWorkspaces :: X ()
ensureWorkspaces = do
  xinesc <- withDisplay getCleanedScreenInfo
  let nscreens = length xinesc
  withWindowSet $ \ss -> do
    let nvisible = length $ W.visible ss
        nhidden = length $ W.hidden ss
        nmissing = max 0 $ nscreens - (1 + nvisible + nhidden)

    when (nmissing > 0) $ do
      mapM_ addWorkspace $ take nmissing $ map (((++) "WS-") . show) [1..]
      rescreen

randr c = c { startupHook = startupHook c >> selectRandrEvents >> updateScreens,
              handleEventHook = handleEventHook c <+> onOutputChanged updateScreens <+> onOutputChanged ensureWorkspaces }
