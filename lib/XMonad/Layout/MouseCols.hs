module XMonad.Layout.MouseCols where

data Cols a =
  Cols
  { _cols :: [Col]
  } deriving (Read, Show)

data Col =
  Col
  { _width :: Rational
  , _rows :: [Rational]
  , _more :: [Rational]
  } deriving (Read, Show)

alloc :: [Int] -> [a] -> [[a]]
alloc (_:[]) as = [as]
alloc (n:ns) as = let (a0, a1) = splitAt n as in a0:(alloc ns a1)

resiz :: [Col] -> [[a]] -> [(Col, [a])]
resiz cs as = map adj $ zip cs (as ++ repeat [])
  where adj (c, a) = let n = length a
                         defined = (_rows c) ++ (_more c)
                         c' = c { _rows = take n $ defined ++ repeat last defined
                                  _more = drop (n - length (_rows c)) (_more c) }
                     in (c', a)

instance LayoutClass Cols a where
  doLayout l r st = do
    let ws = W.integrate st
        wa = alloc (map (_length . _rows) (_cols l)) ws
        cols = resiz (_cols l) wa

    return $ something
