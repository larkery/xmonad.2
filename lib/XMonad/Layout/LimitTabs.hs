module XMonad.Layout.LimitTabs where

data LimitTabs l1 l2 a =
  LT
  { _enableAt :: Maybe Int
  , _limitTo :: Int
  , _masterLayout :: l1 Int
  , _overflowLayout :: l2 a
  }

instance LayoutClass LimitTabs l1 l2 Window where
  description (LT {_enableAt = e, _masterLayout = m, _limitTo = n}) =
    case e of
      Nothing -> description m
      (Just _) -> "Limit" ++ (show n) ++ " " ++ (description m)

  runLayout wsp@(W.Workspace wid state stack) rect =
    let LT { _enableAt = threshold,
              _limitTo = limit,
              _masterLayout = master,
              _overflowLayout = overflow
           } = state
        W.integrate stack
        nWindows = length
    in
