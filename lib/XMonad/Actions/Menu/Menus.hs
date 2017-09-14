{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, Rank2Types #-}

module XMonad.Actions.Menu.Menus (windowMenu, commandMenu, workspaceMenu) where

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

windowMenu = do
  window <- menu def getWindows :: X (Maybe (Choice NamedWindow, Action NamedWindow))
  whenJust window $ \(C {_value = w}, A {_action = a}) -> a w
    where getWindows s = ((filter (matches s . _choiceLabel)) . (map wrap)) <$> allWindows
          allWindows = (gets (W.allWindows . windowset)) >>= mapM getName
          _focus :: Action NamedWindow
          _focus = A {_actionLabel = "focus", _action = \nw -> windows $ W.focusWindow (unName nw)}
          wrap :: NamedWindow -> Choice NamedWindow
          wrap nw = C { _value = nw, _choiceLabel = show nw, _actions = [_focus] }

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
        wrap :: String -> Choice String
        wrap c = C { _value = c, _choiceLabel = c, _actions=[run] }

workspaceMenu = do
  ws <- gets windowset

  let current = W.tag $ W.workspace $ W.current ws
      visible = map (W.tag . W.workspace) $ W.visible ws
      (hiddenW, hiddenEmptyW) = partition (isJust . W.stack) $ W.hidden ws
      (hidden, hiddenEmpty) = (map W.tag hiddenW, map W.tag hiddenEmptyW)

      tags = current:(visible ++ hidden ++ hiddenEmpty)

  command <- menu def (findTag tags)
  whenJust command $ \(C{_value = tag}, A{_action = a}) -> a tag
  where findTag tags s = return $ map wrap $ filter (matches s) tags -- etc.
        wrap tag = C { _value = tag, _choiceLabel = tag, _actions = [_view, _del] }
        _view = A {_actionLabel = "view", _action = windows . W.greedyView}
        _create = A {_actionLabel = "create", _action = addWorkspace}
        _shift = A {_actionLabel = "shift", _action = withFocused . shiftWindowToNew}
        _del = A {_actionLabel = "del", _action = \t -> (windows $ W.view t) >> killAll >> removeWorkspace}
        _rename = A {_actionLabel = "rename", _action = renameWorkspaceByName}
        shiftWindowToNew ws w = do addHiddenWorkspace ws
                                   windows $ W.shiftWin ws w
