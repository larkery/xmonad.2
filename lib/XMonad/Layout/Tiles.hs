{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
module XMonad.Layout.Tiles where

import XMonad
import qualified XMonad.StackSet as W

import qualified Data.Sequence as S
import Data.Sequence (Seq (..), (<|))

--
