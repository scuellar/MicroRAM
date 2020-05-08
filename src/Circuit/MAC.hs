{-# LANGUAGE DataKinds #-}
module Circuit.MAC where

{-
import Clash.Prelude

ma acc (x,y) = acc + x * y

macT acc (x,y) = (acc',o)
  where
    acc' = ma acc (x,y)
    o    = acc

mac = macT <^> 0

topEntity :: (Signal (Signed 9),Signal (Signed 9)) -> Signal (Signed 9)
topEntity = mac
-}
