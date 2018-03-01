{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
module XMonad.Layout.AdjustableTall where

import XMonad
import XMonad.Layout (splitHorizontallyBy)
import qualified XMonad.StackSet as W
import Data.Maybe
import Control.Monad (msum)
import Control.Arrow (first, second)
import Graphics.X11.Xlib.Extras (getWindowAttributes,
                                 WindowAttributes (..))
import Graphics.X11.Xlib.Misc (warpPointer)
import Data.List (findIndex, (\\))
import qualified Data.Map as M
import qualified Debug.Trace as D
import Control.Arrow ( first )

data Edge = L | R | T | B deriving (Read, Show, Typeable, Eq)

data AdjustableTileMessage =
  AdjustTile Int Edge Position |
  ExpandTile Int Rational |
  ResetTiles
  deriving (Read, Show, Typeable)

instance Message AdjustableTileMessage

adjustableTall :: Rational -> Int -> AdjustableTall a
adjustableTall s n = AdjustableTall {
  _hsplit = s
  , _capacity = n
  , _splits = ([], [])
  , _last = ([], [])
  , _lastRect = (Rectangle 0 0 0 0)
  }

data AdjustableTall a =
  AdjustableTall
  { _hsplit   :: !Rational
  , _capacity :: !Int
  , _splits :: !([Rational], [Rational])
  , _last :: !([Rational], [Rational])
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

upd :: [Rational] -> [Rational] -> Int -> ([Rational], [Rational])
upd xs lst n
  | n == length xs = (xs, lst)
  | n > length xs = (scale $ take' n $ (xs ++ lst), drop (n - (length xs)) lst)
  | n < length xs = let (xs0, xs1) = splitAt n xs in (scale $ xs0, lst ++ xs1)
  where take' n xs = take n $ xs ++ (repeat $ if null xs then 1 else (last xs))
        scale xs = xs

instance LayoutClass AdjustableTall a where
  description l = "ATall"

  pureMessage l m = msum [fmap incmastern (fromMessage m),
                          fmap adjust (fromMessage m),
                          fmap resize (fromMessage m)
                         ]
    where
      cap = _capacity l

      resize Shrink = l { _hsplit = snap 8 $ max (1/8) $ (_hsplit l) - (1/8) }
      resize Expand = l { _hsplit = snap 8 $ min (7/8) $ (_hsplit l) + (1/8) }

      snap :: Rational -> Rational -> Rational
      snap r n = (fromIntegral (round $ n * (r :: Rational))) / r

      incmastern (IncMasterN d) = l { _capacity = max 0 (cap + d) }

      adjust ResetTiles = l { _hsplit = (1/2)
                            , _splits = ([], [])
                            , _last = ([], [])
                           }

      adjust  (ExpandTile n r)
        | n < cap = l { _splits = first (expand' n) (_splits l) }
        | otherwise = l { _splits = second (expand' (n - cap)) (_splits l)}
        where a +| b = snap 16 a+b
              a -| b = a --snap 16 (a-b)
              expand' n splits =
                case first reverse $ splitAt n splits of
                  ([], h:(t:tt)) -> (h+|r):(t-|r):tt
                  (u:us, h:(t:tt)) -> (reverse $ (u -| (r/2)):us) ++ (h+|r):(t-|(r/2)):tt
                  (u:us, h:[]) -> (reverse $ (h+|r):(u -| (r)):us)
                  _ -> splits
      adjust (AdjustTile n e px)
        -- adjust the central split:
        | (e == L && n >= cap) || (e == R && n < cap) = l { _hsplit = max (1/16) $ min (15/16) $ p }
          -- adjust top & bottom thing
        | e == T = adjust (AdjustTile (n-1) B px)
        | e == B && n < cap = l { _splits = first (adjustSplit n p) (_splits l) }
        | e == B = l { _splits = second (adjustSplit (n - cap) p) (_splits l) }
          -- do nothing
        | otherwise = l
        where Rectangle sx sy sw sh = _lastRect l
              p | e == L || e == R = snap 32 $ (fromIntegral px - fromIntegral sx) / (fromIntegral sw)
                | otherwise = snap 32 $ (fromIntegral px - fromIntegral sy) / (fromIntegral sh)

      adjustSplit ::  Int -> Rational -> [Rational] -> [Rational]
      adjustSplit n p ss
        | n < 0 = ss
        | otherwise = let (above, below) = splitAt n ss
                          ta = sum above
                          tb = sum below
                          p' = p * (sum ss)
                          a' = max (1/6) p' - ta
                      in case below of
                           a:(b:cs) -> above ++ a':(max (1/6) $ b + a - a'):cs
                           _ -> ss

  doLayout l r st =
    let windows = W.integrate st
        (lws, rws) = windows $> splitAt (_capacity l)
        ml = upd (fst $ _splits l) (fst $ _last l) (length lws)
        mr = upd (snd $ _splits l) (snd $ _last l) (length rws)
        l' = Just $ l { _splits = (fst ml, fst mr), _last = (snd ml, snd mr) }
        l'' = if r /= (_lastRect l)
              then Just $ (fromMaybe l l') { _lastRect = r }
              else l'
        (lss, rss) = _splits $ fromMaybe l l''
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
  floats <- gets (M.keys . W.floating . windowset)
  let (Just n) = findIndex (== w) (windows \\ floats)

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


growTileVertically :: Rational -> X ()
growTileVertically r = do
  windows <- gets (W.integrate' . W.stack . W.workspace . W.current . windowset)
  floats <- gets (M.keys . W.floating . windowset)
  mw <- gets (W.peek . windowset)
  whenJust mw $ \w-> do
    let mn = findIndex (== w) (windows \\ floats)
    whenJust mn $ \n -> sendMessage $ ExpandTile n r
