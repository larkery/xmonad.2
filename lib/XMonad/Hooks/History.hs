module XMonad.Hooks.History (historyHook, orderedWindows) where

import XMonad
import Data.Sequence as Seq
import qualified XMonad.StackSet as SS
import Data.Set as Set
import qualified Data.Foldable as F

import qualified XMonad.Util.ExtensibleState as XS

data HistoryDB = HistoryDB (Maybe Window) -- currently focused window
                           (Seq Window)   -- previously focused windows
               deriving (Read, Show, Typeable)


instance ExtensionClass HistoryDB where
    initialValue  = HistoryDB Nothing Seq.empty
    extensionType = PersistentExtension

-- | Action that needs to be executed as a logHook to maintain the
-- focus history of all windows as the WindowSet changes.
historyHook :: X ()
historyHook = XS.get >>= updateHistory >>= XS.put

-- Updates the history in response to a WindowSet change
updateHistory :: HistoryDB -> X HistoryDB
updateHistory (HistoryDB oldcur oldhist) = withWindowSet $ \ss -> do
  let newcur   = SS.peek ss
      wins     = Set.fromList $ SS.allWindows ss
      newhist  = Seq.filter (flip Set.member wins) (ins oldcur oldhist)
  return $ HistoryDB newcur (del newcur newhist)
  where
    ins x xs = maybe xs (<| xs) x
    del x xs = maybe xs (\x' -> Seq.filter (/= x') xs) x

orderedWindows :: X [Window]
orderedWindows = withWindowSet $ \ss -> do
  (HistoryDB mf h) <- XS.get
  let ins x xs = maybe xs (<| xs) x
      wins = SS.allWindows ss
      winsS = Set.fromList wins
      hwins = Seq.filter (flip Set.member winsS) $ ins mf h
      winsH = Set.fromList $ F.toList $ hwins
      missing = Seq.filter (not . flip Set.member winsH) (Seq.fromList wins)

  return $ F.toList $ hwins >< missing
