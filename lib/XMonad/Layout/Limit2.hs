{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses #-}
module XMonad.Layout.Limit2 (limit2, toggleLimit2) where

import XMonad.Layout.LayoutModifier
import Data.Maybe
import Data.List

import XMonad
import qualified XMonad.StackSet as W

data Limit2 a = Limit2 { _enabled :: Bool } deriving (Show, Read)
data Limit2Msg = ToggleLimit2
  deriving (Typeable)

instance Message Limit2Msg

instance LayoutModifier Limit2 Window where
  modifyLayout (Limit2 {_enabled = e}) ws rect =
    let unmodified = runLayout ws rect in
      case (e, ws) of
        (False, _) -> unmodified
        (_, W.Workspace { W.stack = Nothing }) -> unmodified
        (_, W.Workspace { W.stack = Just (W.Stack _ [] []) }) -> unmodified
        (True, W.Workspace {W.stack = Just (W.Stack f [] d) }) ->
          let ws' = ws { W.stack = Just (W.Stack f [] (take 1 d)) }
          in runLayout ws' rect
        (True, W.Workspace {W.stack = Just (W.Stack f u d) }) ->
          let ws' = ws { W.stack = Just (W.Stack f (drop (length u - 1) u) []) }
          in runLayout ws' rect

  pureMess st m = case fromMessage m of
    (Just ToggleLimit2) -> Just $ st { _enabled = not (_enabled st) }
    _ -> Nothing

  modifierDescription (Limit2 {_enabled = False}) = ""
  modifierDescription (Limit2 {_enabled = True}) = "Limit2"

limit2 = ModifiedLayout (Limit2 {_enabled = False})
toggleLimit2 = sendMessage $ ToggleLimit2
