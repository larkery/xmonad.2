{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses, InstanceSigs #-}
module XMonad.Layout.Flip (flipLayout, toggleFlip) where

import Control.Arrow
import XMonad.Layout.LayoutModifier
import Data.Maybe
import Data.List
import XMonad.Layout.AdjustableTall ( AdjustableTileMessage (..), Edge (..) )

import XMonad
import qualified XMonad.StackSet as W

data Flip a = Flip Bool deriving (Show, Read)
data FlipMsg = ToggleFlip deriving (Typeable)
instance Message FlipMsg

instance LayoutModifier Flip Window where
  modifyLayout (Flip True) ws rect =
    let rflip (Rectangle x y w h) = (Rectangle y x h w) in
      (first (map (second rflip))) <$> runLayout ws (rflip rect)
  modifyLayout (Flip False) ws rect = runLayout ws rect

  handleMessOrMaybeModifyIt (Flip b) msg = do
    let tMesg = (fromMessage msg) :: Maybe AdjustableTileMessage
        fMesg = (fromMessage msg) :: Maybe FlipMsg
        flipEdge L = T
        flipEdge R = B
        flipEdge T = L
        flipEdge B = R

    return $ case (b, fMesg, tMesg) of
               (_, Just ToggleFlip, _) -> Just $ Left $ Flip $ not b
               (True, Nothing, Just (AdjustTile n e p)) -> Just $ Right $ SomeMessage $ AdjustTile n (flipEdge e) p
               _ -> Nothing

  modifierDescription (Flip True) = "Flip"
  modifierDescription _ = ""

flipLayout = ModifiedLayout (Flip False)
toggleFlip = sendMessage $ ToggleFlip
