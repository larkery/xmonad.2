{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, Rank2Types #-}

module XMonad.Actions.Menu.Menus (windowMenu, commandMenu, workspaceMenu, sysMenu, passMenu, minT) where

import XMonad
import XMonad.Actions.Menu hiding (_action)
import XMonad.Util.NamedWindows
import Data.List (isInfixOf, partition, (\\))
import Data.Char (toLower)
import Data.Maybe (isJust, fromMaybe)
import XMonad.Prompt.Shell (getCommands)
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.WindowBringer (bringWindow)
import XMonad.Actions.WithAll (killAll)
import XMonad.Util.Run (safeSpawn, runProcessWithInput)
import System.Directory (getHomeDirectory)
import System.FilePath (takeExtension, dropExtension, combine)
import XMonad.Hooks.History
import XMonad.Hooks.WorkspaceHistoryinHistory) allTags)

  command <- menu (addDown def) (findTag tags)
  whenJust command $ \(C{_value = tag}, A{_action = a}) -> a tag
  where findTag tags s = return $ (new tags s) ++ (map wrap $ filter (matches s) tags)
        new tags s = if s `elem` tags || s == "" then [] else
                       [(C { _value = s, _choiceLabel = "[new] " ++ s, _actions = [_create, _rename, _shift] })]
        wrap tag = C { _value = tag, _choiceLabel = tag, _actions = if tag == minT then [_shift] else [_view, _del, _shift] }
        _view = A {_actionLabel = "view", _action = windows . W.greedyView}
        _create = A {_actionLabel = "create", _action = addWorkspace}
        _shift = A {_actionLabel = "shift", _action = withFocused . shiftWindowToNew}
        _del = A {_actionLabel = "del", _action = \t -> (windows $ W.view t) >> killAll >> removeWorkspace}
        _rename = A {_actionLabel = "rename", _action = renameWorkspaceByName}
        shiftWindowToNew ws w = do addHiddenWorkspace ws
                                   windows $ W.shiftWin ws w

sysMenu k = do
  r <- menu (addDown def) gen :: X (Maybe (Choice (X ()), Action (X ())))
  whenJust r $ \(x, a) -> (_action a) (_value x)
  where gen :: String -> X [Choice (X ())]
        gen s = return $ filter ((starts s) . show) commands
        addDown c = c { _keymap = (k, down):(_keymap c) }
        commands :: [Choice (X ())]
        commands = [com "reload" $ spawn reloadCommand,
                    com "hibernate" $ spawn "systemctl hibernate",
                    com "suspend" $ spawn "systemctl suspend",
                    com "wifi" $ spawn "wpa_gui",
                    com "pass" passMenu
                   ]
        reloadCommand = "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi"
        _run :: Action (X ())
        _run = A { _actionLabel = "", _action = id }
        com :: String -> X () -> Choice (X ())
        com l a = C { _value = a, _choiceLabel = l, _actions = [_run] }

passMenu = do
  h <- io $ getHomeDirectory
  passwordFiles <- io $ runProcessWithInput "find" [ combine h ".password-store"
                                                   ,  "-type" , "f"
                                                   , "-name", "*.gpg"
                                                   , "-printf", "%P\n"] []

  ac <- menu def (gen (map (wrap . unsuffix ".gpg") $ lines passwordFiles))
  whenJust ac $ \(C {_value = p}, A{_action = a}) -> a p
  return ()
  where unsuffix :: String -> String -> String
        unsuffix s i
          | takeExtension i == s = dropExtension i
          | otherwise = i

        gen :: [Choice String] -> String -> X [Choice String]
        gen ps s = return $ filter (matches s . show) ps

        wrap p = C { _value = p, _choiceLabel = p, _actions = [open, user, pass] }
        open = A { _action = \p -> spawn $ "passm -l "++p, _actionLabel = "go" }
        user = A { _action = \p -> spawn $ "passm -f user -p -c "++p, _actionLabel = "both" }
        pass = A { _action = \p -> spawn $ "passm -c -p "++p, _actionLabel = "sel pass" }
