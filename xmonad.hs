{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, Rank2Types #-}

import XMonad
import XMonad.Layout.NoBorders
import XMonad.Util.EZConfig
import XMonad.Layout.LimitWindows
import XMonad.Hooks.ManageDocks
import XMonad.Layout.TwoPane
import XMonad.Hooks.EwmhDesktops
import XMonad.Actions.Menu
import XMonad.Util.NamedWindows
import qualified XMonad.StackSet as W
import Data.List (isInfixOf)
import Data.Char (toLower)
import XMonad.Prompt.Shell (getCommands)

main = xmonad mconfig

mconfig =
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
  ]

instance Options NamedWindow (Action NamedWindow) where
  options _ = [ A "focus" (\nw -> windows $ W.focusWindow (unName nw)) ]

data Command = Command String
instance Show Command where
  show (Command c) = c

instance Options Command (Action Command) where
  options _ = [ A "run" (\(Command c) -> spawn c) ]

data Action a = A String (a -> X())
instance Show (Action a) where
  show (A s _) = s

matches s = isInfixOf s . (map toLower)

windowMenu = do
  window <- menu def getWindows
  whenJust window $ \(w, A _ a) -> a w
    where getWindows s = filter (matches s . show) <$> (allWindows >>= mapM getName)
          allWindows = (gets (W.allWindows . windowset))

commandMenu = do
  allCommands <- io $ getCommands
  command <- menu def (findCommands allCommands)
  whenJust command $ \(c, A _ a) -> a c
  where findCommands cs s = let ms = take 20 $ filter (matches s) cs in
                              return $ if null ms then [Command s] else map Command ms
