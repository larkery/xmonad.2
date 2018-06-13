{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, Rank2Types #-}

module XMonad.Actions.Menu.Menus (windowMenu, commandMenu, workspaceMenu, actionMenu, passMenu, minT) where

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
import XMonad.Hooks.WorkspaceHistory
import XMonad.Hooks.NotifyUrgencyHook (setBorder)
import XMonad.Actions.SpawnOn

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
starts s = (== s) . (map toLower) . (take (length s))
pad s n
  | length s >= n = take n s
  | otherwise = s ++ (take (n - (length s)) $ repeat ' ')

data NTWindow = NTWindow {window :: Window,  name :: String, tag :: String}

instance Show NTWindow where
  show (NTWindow {name = n, tag = t}) = (pad t 8)++"| "++n

minT = "&"

windowMenu cfg k = do
  let addDown c = c { _keymap = (k, down >> holdKey):(_keymap c) }
  window <- menu (addDown cfg {_width = 600}) getWindows :: X (Maybe (Choice NTWindow, Action NTWindow))
  whenJust window $ \(C {_value = w}, A {_action = a}) -> a w
    where getWindows s = do
            ws <- allWindows
            return $ filter (matches s . _choiceLabel) $ map wrap ws

          toNTWindow :: (String, Window) -> X NTWindow
          toNTWindow (t, w) = do n <- getName w
                                 return $ NTWindow w (show n) t

          allWindows :: X [NTWindow]
          allWindows = do
            windowTags <- gets ((concatMap tagWindows) . W.workspaces . windowset)
            history <- orderedWindows
            let historyTags = map (fromMaybe "?" . (flip lookup windowTags)) history
            mapM toNTWindow $ zip historyTags history

          tagWindows :: W.Workspace i l a -> [(a, i)]
          tagWindows (W.Workspace {W.tag = t, W.stack = st}) = map (flip (,) t) $ W.integrate' st

          _focus :: Action NTWindow
          _focus = A {_actionLabel = "focus", _action = \nw -> windows $ W.focusWindow (window nw)}
          _bring = A {_actionLabel = "bring", _action = windows . bringWindow . window }
          _master = A {_actionLabel = "mastr", _action = windows . (\w -> W.swapMaster .
                                                                     (W.focusWindow w) .
                                                                     (bringWindow w)) . window }

          wrap :: NTWindow -> Choice NTWindow
          wrap nw = C { _value = nw, _choiceLabel = show nw, _actions = if tag nw == minT then [_bring, _master] else [_focus, _bring, _master] }

commandMenu cfg = do
  allCommands <- io $ getCommands
  command <- menu cfg (findCommands allCommands)
  whenJust command $ \(C{_value = c}, A {_action = a}) -> a c
  where findCommands cs s = let ms = filter (matches s) cs in
                              return $
                              map wrap $
                              if null ms then [s] else ms
        run :: Action String
        run = A { _actionLabel = "run", _action = \c -> spawnHere c }
        term = A { _actionLabel = "term", _action = \c -> safeSpawn "urxvt" ["-e", c]  }
        wrap :: String -> Choice String
        wrap c = C { _value = c, _choiceLabel = c, _actions=[run, term] }

workspaceMenu cfg key = do
  ws <- gets windowset
  history <- workspaceHistory

  let addDown c = c { _keymap = (key, down >> holdKey):(_keymap c) }

      existing :: [String]
      existing = map W.tag $
        (W.workspace $ W.current ws) :
        (map W.workspace $ W.visible ws) ++
        (filter (isJust . W.stack) $ W.hidden ws)
      visible = (map (W.tag . W.workspace) $ W.visible ws)
      
      exists :: String -> Bool
      exists = flip elem existing
      ahistorical :: String -> Bool
      ahistorical = not . (flip elem history)

      tags = (filter exists history) ++ (filter ahistorical existing)

  command <- menu (addDown cfg) (findTag visible tags)
  whenJust command $ \(C{_value = tag}, A{_action = a}) -> a tag
  where findTag vis tags s = return $ (new tags s) ++ (map (wrap vis) $ filter (matches s) tags)
        new tags s = if s `elem` tags || s == "" then [] else
                       [(C { _value = s, _choiceLabel = "[new] " ++ s, _actions = [_create, _rename, _put, _shift] })]
        wrap vis tag = C { _value = tag, _choiceLabel = if tag `elem` vis then "* " ++ tag else "  " ++ tag, _actions = if tag == minT then [_shift] else [_view, _del, _put, _shift] }
        _view = A {_actionLabel = "view", _action = windows . W.greedyView}
        _create = A {_actionLabel = "create", _action = addWorkspace}
        _put = A {_actionLabel = "go", _action = withFocused . putWindowToNew}
        _shift = A {_actionLabel = "shift", _action = withFocused . shiftWindowToNew}
        _del = A {_actionLabel = "del", _action = \t -> (windows $ W.view t) >> killAll >> removeWorkspace}
        _rename = A {_actionLabel = "rename", _action = renameWorkspaceByName}
        putWindowToNew ws w = do addWorkspace ws
                                 windows $ bringWindow w

        shiftWindowToNew ws w = do addHiddenWorkspace ws
                                   windows $ W.shiftWin ws w


actionMenu cfg k commands' = do
  r <- menu (addDown cfg) gen :: X (Maybe (Choice (X ()), Action (X ())))
  whenJust r $ \(x, a) -> (_action a) (_value x)
  where gen :: String -> X [Choice (X ())]
        gen s = return $ filter ((starts s) . show) commands
        addDown c = c { _keymap = (k, down):(_keymap c) }
        commands :: [Choice (X ())]
        commands = map (uncurry com) commands'
        _run :: Action (X ())
        _run = A { _actionLabel = "", _action = id }
        com :: String -> X () -> Choice (X ())
        com l a = C { _value = a, _choiceLabel = l, _actions = [_run] }

passMenu cfg = do
  h <- io $ getHomeDirectory
  passwordFiles <- io $ runProcessWithInput "find" [ combine h ".password-store"
                                                   ,  "-type" , "f"
                                                   , "-name", "*.gpg"
                                                   , "-printf", "%P\n"] []

  ac <- menu cfg (gen (map (wrap . unsuffix ".gpg") $ lines passwordFiles))
  whenJust ac $ \(C {_value = p}, A{_action = a}) -> a p
  return ()
  where unsuffix :: String -> String -> String
        unsuffix s i
          | takeExtension i == s = dropExtension i
          | otherwise = i

        gen :: [Choice String] -> String -> X [Choice String]
        gen ps s = return $ filter (matches s . show) ps

        passm = safeSpawn "passm"

        wrap p = C { _value = p, _choiceLabel = p, _actions = [open, user, pass] }
        open = A { _action = \p -> (passm ["--open", p]) , _actionLabel = "go" }
        user = A { _action = \p -> passm ["-field",  "user",  "--field",  "password", "--copy", p]
                 , _actionLabel = "both" }
        pass = A { _action = \p -> passm ["--field", "password", "--copy", p]
                 , _actionLabel = "sel pass" }
