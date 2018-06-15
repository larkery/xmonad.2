{-# LANGUAGE FlexibleContexts #-}
module XMonad.Hooks.NotifyUrgencyHook (showUrgent, notifyUrgent, setBorder) where

import XMonad
import Control.Monad
import qualified XMonad.StackSet as W
import XMonad.Util.NamedWindows
import XMonad.Hooks.UrgencyHook
import XMonad.Util.Run
import XMonad.Actions.WindowBringer

data LibNotifyUrgencyHook = LibNotifyUrgencyHook deriving (Read, Show)
instance UrgencyHook LibNotifyUrgencyHook where
    urgencyHook LibNotifyUrgencyHook w = do
      name <- getName w
      wset <- gets windowset
      let Just idx = W.findTag w wset
      setBorder "red" w
      safeSpawn "notify-send" ["-u", "critical", (show name) ++ " urgent on " ++ idx, "-a", "xmonad"]

setBorder c w = withDisplay $ \d -> io $ do
  g <- initColor d c
  whenJust g $ \g' -> setWindowBorder d w g'

notifyUrgent conf = withUrgencyHookC hook uc conf
  where hook = LibNotifyUrgencyHook
        uc = urgencyConfig { suppressWhen = Focused , remindWhen = Every 120 }

showUrgent = do
  us <- readUrgents
  case us of
    [] -> return ()
    x:_ -> do
      t <- gets ((W.findTag x) . windowset)
      
      whenJust t $ \t -> windows $ case t of
        "&" -> bringWindow x
        _ -> W.focusWindow x
