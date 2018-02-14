{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}

module XMonad.Layout.Columns where

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Layout (mirrorRect)

import Control.Monad (msum)

import qualified Debug.Trace as D

data Col = Col
  { _right :: Rational
  , _bottom :: [Rational]
  } deriving (Read, Show)

data CState a =
  CState { _cols :: [Col] } deriving (Read, Show)

data Edge = L | R | T | B deriving (Read, Show, Typeable, Eq)

data CMessage =
  Drag Int Edge Position
  deriving (Typeable)

instance Message CMessage

alloc :: [Col] -> [a] -> [(Col, [a])]
alloc _ [] = []
alloc (c:[]) ws = [(c, ws)]
alloc (c:cs) ws =
  let (w0, w1) = splitAt (length $ _bottom c) ws in
    (c, w0):(alloc cs w1)

column :: Rectangle -> (Rational, Rational) -> Rectangle
column (Rectangle sx sy sw sh) (l, r) =
  let lhs = (fromIntegral sx) + floor (l * fromIntegral sw)
      width = floor $ (fromIntegral sw) * (r -l)
  in Rectangle lhs sy width sh

row s rs = mirrorRect $ column (mirrorRect s) rs

instance LayoutClass CState Window where
  description l = "Tall"

  doLayout l r st =
    let windows = W.integrate st
        as = alloc (_cols l) windows
        toBounds ns = let t = sum ns
                          r = map (flip (/) t) ns
                          i = scanl (+) 0 r
                      in zip i $ drop 1 i
        colsRects = zip as $ map (column r) $ toBounds $ map (_right . fst) as
        cellsRects = flip concatMap colsRects $ \((c, ws), r) ->
          zip ws $ map (row r) $ toBounds $ take (length ws) $ (_bottom c) ++ (repeat 1)
    in return $ (cellsRects, Nothing)

  pureMessage l m =
      let (Rectangle sx sy sw sh) = _lastRect l
          drag (Drag n e p)
            | e == L || e == R = dragColumn n e $ (fromIntegral $ p - sx) / (fromIntegral sw)
            | e == T || e == B = dragRow n e $ (fromIntegral $ p - sy) / (fromIntegral sh)
          dragColumn n e p = l
          dragRow n e p = l
      in msum [drag <$> (fromMessage m)]


cols = CState { _cols = [ Col {_right = 1, _bottom = [1]}
                        , Col {_right = 1, _bottom = []}]}
