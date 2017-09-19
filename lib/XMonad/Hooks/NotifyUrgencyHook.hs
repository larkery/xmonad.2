{-# LANGUAGE FlexibleContexts #-}
module XMonad.Hooks.NotifyUrgencyHook (notifyUrgent) where

import XMonad
import Control.Monad
import qualified XMonad.StackSet as W
import XMonad.Util.NamedWindows
import XMonad.Hooks.UrgencyHook
import XMonad.Util.Run

data LibNotifyUrgencyHook = LibNotifyUrgencyHook deriving (Read, Show)
instance UrgencyHook LibNotifyUrgencyHook where
    urgencyHook LibNotifyUrgencyHook w = do
      name <- getName w
      wset <- gets windowset
      let Just idx = W.findTag w wset
      setBorder "red" w
      when (not $ idx `elem` (map (W.tag . W.workspace) $ (W.current wset):(W.visible wset))) $ do
        safeSpawn "notify-send" [(show name) ++ " urgent on " ++ idx, "-a", "xmonad"]

setBorder c w = withDisplay $ \d -> io $ do
  g <- initColor d c
  whenJust g $ \g' -> setWindowBorder d w g'

notifyUrgent conf = withUrgencyHookC hook uc conf
  where hook = LibNotifyUrgencyHook
        uc = urgencyConfig { suppressWhen = Focused , remindWhen = Every 120 }
