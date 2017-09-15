{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, Rank2Types #-}

module XMonad.Actions.Menu.Menus (windowMenu, commandMenu, workspaceMenu, sysMenu, passMenu) where

import XMonad
import XMonad.Actions.Menu hiding (_action)
import XMonad.Util.NamedWindows
import Data.List (isInfixOf, partition)
import Data.Char (toLower)
import Data.Maybe (isJust)
import XMonad.Prompt.Shell (getCommands)
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.WindowBringer (bringWindow)
import XMonad.Actions.WithAll (killAll)
import XMonad.Util.Run (safeSpawn, runProcessWithInput)
import System.Directory (getHomeDirectory)
import System.FilePath (takeExtension, dropExtension, combine)

import qualified XMonad.StackSet as W

data Choice a = C { _value :: a, _choiceLabel :: String, _actions :: [Action a] }
data Action a = A { _actionLabel :: String, _action :: a -> X () }

instance Show (Choice a) where
  show (C {_choiceLabel = l}) = l

instance Show (Action a) where
  show (A {_actionLabel = l}) = l

instance Options (Choice a) (Action a) where
  options (C {_actions = as}) = as

matches s = isInfixOf s . (map toLower)

data NTWindow = NTWindow {window :: Window,  name :: String, tag :: String}

instance Show NTWindow where
  show (NTWindow {name = n, tag = t}) = "["++t++"] "++n

windowMenu  k = do
  let addDown c = c { _keymap = (k, down):(_keymap c) }
  window <- menu (addDown def) getWindows :: X (Maybe (Choice NTWindow, Action NTWindow))
  whenJust window $ \(C {_value = w}, A {_action = a}) -> a w
    where getWindows s = ((filter (matches s . _choiceLabel)) . (map wrap)) <$> allWindows
          toNTWindow :: (String, Window) -> X NTWindow
          toNTWindow (t, w) = do n <- getName w
                                 return $ NTWindow w (show n) t

          allWindows :: X [NTWindow]
          allWindows = (gets ((concatMap tagWindows) . W.workspaces . windowset)) >>= (mapM toNTWindow)

          tagWindows :: W.Workspace i l a -> [(i, a)]
          tagWindows (W.Workspace {W.tag = t, W.stack = st}) = map ((,) t) $ W.integrate' st

          _focus :: Action NTWindow
          _focus = A {_actionLabel = "focus", _action = \nw -> windows $ W.focusWindow (window nw)}
          _bring = A {_actionLabel = "bring", _action = windows . bringWindow . window }
          _master = A {_actionLabel = "mastr", _action = windows . (\w -> W.swapMaster .
                                                                     (W.focusWindow w) .
                                                                     (bringWindow w)) . window }

          wrap :: NTWindow -> Choice NTWindow
          wrap nw = C { _value = nw, _choiceLabel = show nw, _actions = [_focus, _bring, _master] }

commandMenu = do
  allCommands <- io $ getCommands
  command <- menu def (findCommands allCommands)
  whenJust command $ \(C{_value = c}, A {_action = a}) -> a c
  where findCommands cs s = let ms = take 20 $ filter (matches s) cs in
                              return $
                              map wrap $
                              if null ms then [s] else ms
        run :: Action String
        run = A { _actionLabel = "run", _action = \c -> spawn c }
        term = A { _actionLabel = "term", _action = \c -> safeSpawn "urxvt" ["-e", c]  }
        wrap :: String -> Choice String
        wrap c = C { _value = c, _choiceLabel = c, _actions=[run, term] }

workspaceMenu key = do
  ws <- gets windowset

  let addDown c = c { _keymap = (key, down):(_keymap c) }
      current = W.tag $ W.workspace $ W.current ws
      visible = map (W.tag . W.workspace) $ W.visible ws
      (hiddenW, hiddenEmptyW) = partition (isJust . W.stack) $ W.hidden ws
      (hidden, hiddenEmpty) = (map W.tag hiddenW, map W.tag hiddenEmptyW)

      tags = current:(visible ++ hidden ++ hiddenEmpty)

  command <- menu (addDown def) (findTag tags)
  whenJust command $ \(C{_value = tag}, A{_action = a}) -> a tag
  where findTag tags s = return $ (new tags s) ++ (map wrap $ filter (matches s) tags)
        new tags s = if s `elem` tags || s == "" then [] else
                       [(C { _value = s, _choiceLabel = "[new] " ++ s, _actions = [_create, _rename, _shift]})]
        wrap tag = C { _value = tag, _choiceLabel = tag, _actions = [_view, _del, _shift] }
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
        gen s = return $ filter ((matches s) . show) commands
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
        open = A { _action = \p -> spawn $ "passm -l "++p, _actionLabel = "open" }
        user = A { _action = \p -> spawn $ "passm -f user -p -c "++p, _actionLabel = "u/p" }
        pass = A { _action = \p -> spawn $ "passm -c -p "++p, _actionLabel = "pass" }
