{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
module XMonad.Layout.AdjustableTall where

import XMonad
import XMonad.Layout (splitHorizontallyBy)
import qualified XMonad.StackSet as W
import Data.Maybe
import Control.Monad (msum)
import Graphics.X11.Xlib.Extras (getWindowAttributes,
                                 WindowAttributes (..))
import Graphics.X11.Xlib.Misc (warpPointer)
import Data.List (findIndex)
import qualified Debug.Trace as D

data Edge = L | R | T | B deriving (Read, Show, Typeable, Eq)

data AdjustableTileMessage =
  AdjustTile Int Edge Position
  deriving (Read, Show, Typeable)

instance Message AdjustableTileMessage

ajustableTall :: Rational -> Int -> AdjustableTall a
ajustableTall s n = AdjustableTall s n [] [] (Rectangle 0 0 0 0)

data AdjustableTall a =
  AdjustableTall
  { _hsplit   :: !Rational
  , _capacity :: !Int
  , _leftSplits :: ![Rational]
  , _rightSplits :: ![Rational]
  , _lastRect :: !Rectangle
  } deriving (Read, Show)

($>) = flip ($)

norm xs = let t = sum xs in map (flip (/) t) xs

splitRow :: Rectangle -> -- column rectangle
            Rational -> -- vsplit
            (Rational, [Rectangle]) ->
            (Rational, [Rectangle]) -- accumulator
splitRow (Rectangle sx sy sw sh) v (v0, acc) =
  (v1, wr:acc)
  where
    v1 = v + v0
    wy :: Position
    wy = sy + (fromIntegral $ floor $ (fromIntegral sh) * v0)
    wh :: Dimension
    wh = fromIntegral $ floor $ (fromIntegral sh) * v
    wr = Rectangle sx wy sw wh

updateSplits :: [Rational] -> [Rational] -> -- existing splits
                Int -> Int -> -- desired sizes
                Maybe ([Rational], [Rational])
updateSplits ls rs nl nr
  | nl == length ls && nr == length rs = Nothing
  | otherwise = Just (upd ls nl, upd rs nr)
  where upd xs n = take n $ xs ++ repeat 1

instance LayoutClass AdjustableTall a where
  description l = "ATall"

  pureMessage l m = msum [fmap incmastern (fromMessage m),
                          fmap adjust (fromMessage m),
                          fmap resize (fromMessage m)
                         ]
    where
      cap = _capacity l

      resize Shrink = l { _hsplit = snap $ max 0 $ (_hsplit l) - (1/8) }
      resize Expand = l { _hsplit = snap $ min 1 $ (_hsplit l) + (1/8) }

      snap :: Rational -> Rational
      snap n = (fromIntegral (round $ n * (16 :: Rational))) / 16

      incmastern (IncMasterN d) = l { _capacity = max 0 (cap + d) }

      adjust (AdjustTile n e px)
        -- adjust the central split:
        | (e == L && n >= cap) || (e == R && n < cap) = l { _hsplit = p }
          -- adjust top & bottom thing
        | e == T = adjust (AdjustTile (n-1) B px)
        | e == B && n < cap = l { _leftSplits = adjustSplit (_leftSplits l) n p }
        | e == B = l { _rightSplits = adjustSplit (_rightSplits l) (n - cap) p }
          -- do nothing
        | otherwise = l
        where Rectangle sx sy sw sh = _lastRect l
              p | e == L || e == R = (fromIntegral px - fromIntegral sx) / (fromIntegral sw)
                | otherwise = (fromIntegral px - fromIntegral sy) / (fromIntegral sh)

      adjustSplit :: [Rational] -> Int -> Rational -> [Rational]
      adjustSplit ss n p
        | n < 0 = ss
        | otherwise = let (above, below) = splitAt n ss
                          ta = sum above
                          tb = sum below
                          p' = p * (fromIntegral $ length ss)
                          a' = p' - ta
                      in case below of
                           a:(b:cs) -> above ++ a':(b + a - a'):cs
                           _ -> ss

  doLayout l r st =
    let windows = W.integrate st
        (lws, rws) = windows $> splitAt (_capacity l)
        msplits =
          updateSplits (_leftSplits l) (_rightSplits l) (length lws) (length rws)
        l' = ((\(ls, rs) -> l {_leftSplits = ls, _rightSplits = rs}) <$> msplits)
        l'' = if r /= (_lastRect l)
              then Just $ (fromMaybe l l') { _lastRect = r }
              else l'
        (lss, rss) = (\s -> (_leftSplits s, _rightSplits s)) $ fromMaybe l l''
        cutup rect splits = reverse $ snd $ foldl (flip $ splitRow rect) (0, []) $ norm splits
    in return $ flip (,) l'' $
       if (null lws) || (null rws)
       then
         -- tile windows into one column
         let rects = cutup r (if null lss then rss else lss)
         in zip windows rects
       else -- tile windows into two columns
         let (leftRect, rightRect) = splitHorizontallyBy (_hsplit l) r
             rects = (cutup leftRect lss) ++ (cutup rightRect rss)
         in zip windows rects

-- This has a bug in that it uses the whole screen rect when the
-- layout sees a reduced rect because of e.g. struts
mouseResizeTile :: Rational -> (Window -> X ())  -> Window -> X ()
mouseResizeTile border fallback w =
  whenX (isClient w) $
  withDisplay $ \dpy -> do
  wa <- io $ getWindowAttributes dpy w
  (_, _, _, ox', oy', _, _, _) <- io $ queryPointer dpy w
  windows <- gets (W.integrate' . W.stack . W.workspace . W.current . windowset)
  let (Just n) = findIndex (== w) windows

  -- need screen dimensions to figure out proportions

  let wx = fromIntegral $ wa_x wa
      wy = fromIntegral $ wa_y wa
      ww = fromIntegral $ wa_width wa
      wh = fromIntegral $ wa_height wa
      ox = fromIntegral ox'
      oy = fromIntegral oy'

      -- drag handler needs to divide?
      drag mouse wpos wdim  e1 e2
        | mouse - wpos < (wdim * border) =
            (True, 0, \px -> sendMessage $ AdjustTile n e1 px)
        | (wpos + wdim) - mouse < (wdim * border) =
            (True, wdim, \px -> sendMessage $ AdjustTile n e2 px)
        | otherwise =
            (False, mouse - wpos, \px -> return ())

      (hitX, warpx, dragX) = drag ox wx ww L R
      (hitY, warpy, dragY) = drag oy wy wh T B

      dragHandler x y = do
        dragX x
        dragY y
      stopHandler = return ()
  if hitX || hitY
    -- warp pointer here
    then do io $ warpPointer dpy none w 0 0 0 0 (floor warpx) (floor warpy)
            mouseDrag dragHandler stopHandler
    else fallback w