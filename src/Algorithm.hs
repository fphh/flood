{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}

module Algorithm where

import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Control.Monad (when)
import Control.Monad.ST

import qualified Data.Vector.Mutable as MVec
import qualified Data.Vector.Generic as GVec
import Data.Vector (Vector)
import qualified Data.Vector as Vec

import System.Random


data Color =
  Green | Red | Blue | Yellow | DarkOrange deriving (Eq, Show, Enum, Generic, NFData)

toColour :: Int -> IO Color
toColour = return . ([Green, Red, Blue, Yellow, DarkOrange] !!)

type Matrix a = Vector (Vector a)

row, col :: Int
row = 16
col = 12

center :: (Int, Int)
center = (0, 0)

randomVector :: IO (Vector Color)
randomVector = Vec.replicateM col (randomRIO (0, 4) >>= toColour)

randomMatrix :: IO (Matrix Color)
randomMatrix = Vec.replicateM row randomVector

endCondition :: Vector (Vector Color) -> Color -> Bool
endCondition mat color =
  GVec.foldr f True mat
  where f v acc = GVec.foldr g True v && acc
        g c acc = c == color && acc

type STMatrix s a = Vec.MVector s (Vec.MVector s a)

freeze :: STMatrix s a -> ST s (Matrix a)
freeze mat = GVec.unsafeFreeze mat >>= GVec.mapM GVec.unsafeFreeze
{-# INLINE freeze #-}

thaw :: Matrix a -> ST s (STMatrix s a)
thaw mat = GVec.mapM GVec.unsafeThaw mat >>= GVec.unsafeThaw
{-# INLINE thaw #-}

update :: STMatrix s a -> (Int, Int) -> a -> ST s ()
update mat (r, c) x = MVec.unsafeRead mat r >>= \v -> MVec.unsafeWrite v c x
{-# INLINE update #-}

look :: Matrix a -> (Int, Int) -> a
look mat (r, c) = GVec.unsafeIndex (GVec.unsafeIndex mat r) c
{-# INLINE look #-}

floodFill :: Matrix Color -> Color -> (Int, Int) -> Matrix Color
floodFill mat replacement node = do
  let target = GVec.unsafeIndex (GVec.unsafeIndex mat (fst center)) (snd center)
      m :: ST s (Matrix Color)
      m = do
        hs <- thaw mat
        let go n@(r, c) = do
              let color = GVec.unsafeIndex (GVec.unsafeIndex mat r) c
              when (color == target) $ do
                update hs n replacement
                when (r > 0) $ go (r-1, c)
                when (r < row-1) $ go (r+1, c)
                when (c > 0) $ go (r, c-1)
                when (c < col-1) $ go (r, c+1)
                
        go node
        freeze hs

  xs <- runST m
  return xs
