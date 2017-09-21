{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses #-}
module XMonad.Layout.Limit2 (limit2, toggleLimit2, rotateOtherWindow) where

import XMonad.Layout.LayoutModifier
import Data.Maybe
import Data.List

import XMonad
import qualified XMonad.StackSet as W

data Limit2 a = Limit2 { _enabled :: Bool, _otherWindow :: Maybe a } deriving (Show, Read)
data Limit2Msg = ToggleLimit2 | SetOther (Maybe Window -> Maybe Window)
  deriving (Typeable)

instance Message Limit2Msg

instance LayoutModifier Limit2 Window where
  modifyLayoutWithUpdate st@(Limit2 {_enabled = e, _otherWindow = o}) ws rect =
    let unmodified = (flip (,) Nothing) <$> runLayout ws rect in
      case (e, ws) of
        (False, _) -> unmodified
        (_, W.Workspace { W.stack = Nothing }) -> unmodified
        (_, W.Workspace { W.stack = Just (W.Stack _ [] []) }) -> unmodified
        (True, W.Workspace {W.stack = Just (W.Stack f [] d) }) ->
          let d'  = take 1 $ (maybeToList o ++ (take 1 d)) `intersect` d
              ws' = ws { W.stack = Just (W.Stack f [] d') }
              st' = Just $ st { _otherWindow = listToMaybe d' }
          in (flip (,) st') <$> runLayout ws' rect
        (True, W.Workspace {W.stack = Just (W.Stack f u d) }) ->
          let ws' = ws { W.stack = Just (W.Stack f (drop (length u - 1) u) []) }
              st' = Just $ st { _otherWindow = Just f }
          in (flip (,) st') <$> runLayout ws' rect

  pureMess st m = case fromMessage m of
    (Just ToggleLimit2) -> Just $ st { _enabled = not (_enabled st) }
    (Just (SetOther f)) -> Just $ st { _otherWindow = f $ (_otherWindow st) }
    _ -> Nothing

  modifierDescription (Limit2 {_enabled = False}) = ""
  modifierDescription (Limit2 {_enabled = True}) = "Limit2"

limit2 = ModifiedLayout (Limit2 {_enabled = False, _otherWindow = Nothing})

toggleLimit2 = sendMessage $ ToggleLimit2
changeOtherWindow = sendMessage . SetOther

rotateOtherWindow n = do
  windows <- gets (W.integrate' . W.stack . W.current . windowset)
  let nextWindow (Nothing) = listToMaybe $ take 1 windows
      nextWindow (Just w) = case (findIndex (== w) windows) of
        Nothing -> listToMaybe windows
        (Just i) -> listToMaybe (drop ((i + n) `mod` (length windows)) windows)

  changeOtherWindow nextWindow
