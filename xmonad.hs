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
import XMonad.Actions.CycleWS
import XMonad.Actions.DwmPromote
import Data.Maybe
import Data.List ( (\\), intersect, delete )
import XMonad.Util.WorkspaceCompare ( getSortByTag )
import XMonad.Hooks.NotifyUrgencyHook
import XMonad.Hooks.UrgencyHook (clearUrgents)
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
import XMonad.Layout.Tabbed
import XMonad.Layout.LayoutModifier (LayoutModifier, ModifiedLayout (..))
import XMonad.Layout.Simplest (Simplest (..))
import XMonad.Layout.Decoration(Decoration, DefaultShrinker)
import XMonad.Hooks.FadeInactive (setOpacity)

import qualified Data.Map.Strict as M
import qualified XMonad.Actions.FlexibleManipulate as Flex
import qualified XMonad.StackSet as W
import XMonad.Layout.Renamed
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import qualified XMonad.Util.Stack as Z

import qualified XMonad.Util.Hint as Hint

import qualified Debug.Trace as D

main = xmonad mconfig

fg2 = "magenta"
fg =  "#006699"
bg = "#f6f6f6"
nborder = "#555"

cl c = c {_background = "darkmagenta"}

getSortByTag' = ((.) minTLast) <$> getSortByTag
  where minTLast [] = []
        minTLast (x:xs)
          | (W.tag x) == minT = xs ++ [x]
          | otherwise = x:(minTLast xs)

addHistory c = c { logHook = (logHook c) >> historyHook >> workspaceHistoryHook }

addLog c = c
  {
    startupHook = do startupHook c
                     -- delete all empty hidden workspaces
                     windows $ \ss -> ss {W.hidden = filter (isJust . W.stack) (W.hidden ss)}
                     ensureWorkspaces

  }

specialWindows c = c { manageHook = (manageHook c) <+> manageSpawn <+> rules}
  where rules = composeAll [ isDialog --> doFloatSnap
                           , transience'
                           , className =? "Pinentry-gtk-2" --> doFloatSnap
                           , className =? "Pinentry" --> doFloatSnap
                           , className =? "Xmessage" --> doFloatSnap
                           , className =? "Yad" --> doFloatSnap
                           , className =? "password-input" --> doFloatSnap <+> op 0.8
                           , className =? "XClock" --> doFloatSnap <+> op 0.6
                           , className =? "Dunst" --> doIgnore
                           ]
        op x = (ask >>= \w -> liftX (setOpacity w x) >> idHook)
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
  hint $
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
    , workspaces = ["sys", "mail"]
    } `additionalKeysP` mkeys
    `additionalMouseBindings`
    [((mod4Mask, 1), \w -> do float <- isFloating w
                              let flex x | x < 0.2 = 0
                                  flex x | x > 0.8 = 1
                                         | otherwise = 0.5
                              if float
                                then (focus w >> Flex.mouseWindow flex w >> ifClick (windows $ W.sink w))
                                else mouseResizeTile (1/4) mouseMoveWindow w
     )]
  )

_layout = trackFloating $
          fullscreenToggleStruts $
          avoidStruts $
          smartBorders $
          renamed [CutWordsLeft 2] $
          smartSpacing 3 $
          mkToggle (single FULL) $
          flipLayout $
          tall ||| big
  where tall = nm "Tall" $
               adjustableTall (1/2) 1
        nm n = renamed [Replace n]
        big = (nm "Big" $ Mirror (OneBig (1/2 + 1/8) (1/2 + 1/8)))

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
  , ( "M-f", sendMessage $ Toggle FULL )
  , ( "M-v", sendMessage NextLayout )
  , ( "M-b", sendMessage ToggleStruts )
  , ( "M-x", commandMenu (cl def) )
  , ( "M-;", workspaceMenu (cl def) "M-;" )
  , ( "M-q", sysMenu "M-q" )

  , ( "M-r", toggleFlip )
  , ( "M-z", sendT )
  , ( "M-y", bringMenu (cl def) minT "M-y" )
  , ( "M-<Return>", withMaster (const $ windows W.shiftMaster) (windows . W.focusWindow) >> warp )

  , ( "<XF86AudioRaiseVolume>", spawn "pamixer -i 5" )
  , ( "<XF86AudioLowerVolume>", spawn "pamixer -d 5" )
  , ( "<XF86AudioMute>", spawn "pamixer -t" )
  , ( "<XF86MonBrightnessUp>", spawn "xbacklight -inc 10" )
  , ( "<XF86MonBrightnessDown>", spawn "xbacklight -dec 10" )
  , ( "<XF86Launch1>", spawn "touchpad" )

  , ( "M-o", nextScreen >> warp )
  , ( "M-n", windows $ W.focusDown )

  , ( "M-'", sendMessage ResetTiles)
  , ( "M-g", (selectWindowColors bg "darkorange") >>= (flip whenJust (windows . bringToMaster)) >> warp )
  , ( "M-a", (selectWindowColors bg "purple") >>= (flip whenJust swapFocused) >> warp )
  , ( "M-S-k", (selectWindowColors bg "red") >>= (flip whenJust killWindow) )
  , ( "M-u", showUrgent )
  , ("M-S-u", clearUrgents)
  , ( "M-m", withMaster (windows . W.focusWindow) (windows . W.focusWindow) >> warp )

  , ( "M-p", windows $ W.focusUp )
  , ( "M-S-n", windows $ W.swapDown )
  , ( "M-S-p", windows $ W.swapUp )
  , ( "M-k", kill )
  , ( "M-M1-n", findWorkspace getSortByTag' Next interestingWS 1 >>= (windows . W.greedyView))
  , ( "M-M1-p", findWorkspace getSortByTag' Prev interestingWS 1 >>= (windows . W.greedyView))

  , ("M-M1-o", swapNextScreen)
  , ("M-S-o", shiftNextScreen)
  , ("M-=", growTileVertically (1/4))
  , ("M--", growTileVertically (-1/4))

  , ("M-S-/", spawn "notify-send \"Check mail\"; VERBOSE=1 notmuch new")
  ] ++
  concat [ [ ("M-" ++ show n, view n) , ("M-S-" ++ show n, shiftTo n)] | n <- [1 .. 9] ]
  where bringToMaster :: Window -> WindowSet -> WindowSet
        bringToMaster w = W.shiftMaster . (W.focusWindow w) . (bringWindow w)

        swapFocused :: Window -> X ()
        swapFocused w = withFocused $ \f -> (windows (swapWindows f w))

        swapWindows w1 w2 s = W.mapWorkspace (\w -> w {W.stack = Z.mapZ_ exch (W.stack w)}) s
          where exch a
                  | a == w1 = w2
                  | a == w2 = w1
                  | otherwise = a

        withMaster a b = do
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
withNthNEWorkspace job wnum = do ws' <- nonEmptyNames
                                 let ws = filter (/= "&") ws'
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

hint c = c { startupHook = startupHook c >> Hint.grabPress
           , handleEventHook = handleEventHook c <+> (Hint.eventHook (Hint.content nonEmptyNames)) }
