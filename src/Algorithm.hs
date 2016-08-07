{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Algorithm where

import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

import qualified Data.Set as Set
import Data.Set (Set)

import qualified Data.Vector as Vec
import Data.Vector (Vector)

import qualified Data.List as List

import System.Random

import Debug.Trace

data Color =
  Green | Red | Blue | Yellow | Pink deriving (Eq, Show, Enum, Generic, NFData)

toColour :: Int -> IO Color
toColour = return . ([Green, Red, Blue, Yellow, Pink] !!)

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

randomIntVector :: IO (Vector Int)
randomIntVector = Vec.replicateM col (randomRIO (0, 4))

randomIntMatrix :: IO (Matrix Int)
randomIntMatrix = Vec.replicateM row randomIntVector

newMat :: IO (Matrix Int)
newMat = do
  mat <- randomIntMatrix
  prettyMatrix mat
  return mat


prettyMatrix :: (Show a) => Matrix a -> IO ()
prettyMatrix mat =
  let xs = Vec.toList mat
  in mapM_ print xs

look :: Matrix a -> (Int, Int) -> a
look mat (r, c) = (mat Vec.! r) Vec.! c



solution :: ((Int, Int) -> Bool) -> (Int, Int) -> Set (Int, Int)
solution cond start = snd $ head $
  let env (r, c) = filter p [(r-1, c), (r, c-1), (r, c+1), (r+1, c)]
        where p (a, b) = 0 <= a && a < row && 0 <= b && b < col

      step v@([], _) = v
      step (cell:xs, visited) =
        let as = env cell
            q x = not (Set.member x visited)
            bs = filter (\x -> cond x && q x) as
            newVisited = List.foldr Set.insert visited bs
        in (bs ++ xs, newVisited)

      end = not . null . fst
      
  in dropWhile end $ iterate step ([start], Set.singleton start)


endCondition :: (Eq a) => Matrix a -> a -> Bool
endCondition mat color =
  Vec.foldr f True mat
  where f v acc = Vec.foldr g True v && acc
        g c acc = c == color && acc



