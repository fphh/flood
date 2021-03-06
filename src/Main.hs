{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}


module Main where

import Control.Monad
import Control.DeepSeq (NFData)

import GHC.Generics (Generic)

import qualified Data.Aeson as Aeson

import Data.Colour (Colour)
import qualified Data.Colour.Names as CN
import qualified Data.Colour.SRGB as SRGB

import qualified Data.Vector as Vec
import Data.Vector (Vector)

import React.Flux

import Algorithm

toHex :: Color -> String
toHex c = SRGB.sRGB24show $
  case c of
   Green -> (CN.limegreen :: Colour Double)
   Red -> CN.firebrick
   Blue -> CN.dodgerblue
   Yellow -> CN.yellow
   DarkOrange -> CN.darkorange

data AppState = AppState {
  matrix :: Maybe (Matrix Color, Int),
  gameOver :: Bool,
  bestScore :: Maybe Int
  }

data Action =
  CellClicked Color
  | InitGame
  deriving (Generic, NFData)

newBestScore :: Ord a => Maybe (t, a) -> Maybe a -> Maybe a
newBestScore xs bs =
  case (xs, bs) of
   (Nothing, Nothing) -> Nothing
   (Just (_, x), Nothing) -> Just x
   (Nothing, x) -> x
   (Just (_, x), Just y) -> Just (min x y)

changeColors :: Matrix Color -> Color -> Int -> Maybe Int -> AppState
changeColors mat newColor cnt bs = 
  let newMat = floodFill mat newColor center
      ec = endCondition newMat newColor
      res = Just (newMat, cnt+1)
      newBs = if ec then newBestScore res bs else bs
  in AppState res ec newBs

instance StoreData AppState where
    type StoreAction AppState = Action

    transform (CellClicked _) appState@(AppState _ True _) =
      return appState
      
    transform (CellClicked color) appState@(AppState (Just (mat, cnt)) _ bs) = do
      case look mat center /= color of
       True -> return $ changeColors mat color cnt bs
       False -> return appState
       
    transform InitGame (AppState xs _ bs) = do
      mat <- randomMatrix
      return $ AppState (Just (mat, 0)) False (newBestScore xs bs)
         
    transform _ _ = do
      error "You should never be here!"

store :: ReactStore AppState
store = mkStore $ AppState Nothing False Nothing

dispatch :: Action -> [SomeStoreAction]
dispatch a = [SomeStoreAction store a]

renderCell :: ReactView (Int, Color)
renderCell = defineView "cell" $ \(idx, color) ->
  let c = Aeson.object [ "backgroundColor" Aeson..= toHex color ]
  in td_ [ "className" $= "cell"
         , "style" @= c
         , "key" @= idx
         , onClick $ \_ _ -> dispatch (CellClicked color) ] (return ())


renderCell_ :: Int -> Color -> ReactElementM ViewEventHandler ()
renderCell_ idx color = viewWithKey renderCell idx (idx, color) mempty


renderRow :: ReactView (Int, Vector Color)
renderRow =  defineView "row" $ \(idx, vec) ->
  tr_ [ "className" $= "row"
      , "key" @= idx ] $ Vec.imapM_ renderCell_ vec

renderRow_ :: Int -> Vector Color -> ReactElementM ViewEventHandler ()
renderRow_ idx vec = viewWithKey renderRow idx (idx, vec) mempty


renderGame_ :: Matrix Color -> ReactElementM ViewEventHandler ()
renderGame_ mat = do
  table_ [ "className" $= "matrix" ]
  $ tbody_ []
  $ Vec.imapM_ renderRow_ mat

renderHeader_ :: ReactElementM ViewEventHandler ()
renderHeader_ = do
  h1_ [] "Flood"
  p_ [] "Click on a color of your choice. This will fill the area starting from the left upper corner with the choosen color. How many clicks do you need until only one color is left?"

  
renderCounter_ :: Int -> Bool -> Maybe Int -> ReactElementM ViewEventHandler ()
renderCounter_ cnt go bs =
  p_ [ "className" $= "counter" ] $ do
    span_ [ "className" $= "line-start" ] $ "You needed "
    b_ [] $ elemShow cnt
    span_ [] $ elemString $ " move" ++ (if cnt == 1 then "" else "s") ++ " until now."
    case bs of
     Just x -> do
       br_ []
       span_ [ "className" $= "line-start" ] $ " Your minimal score is "
       b_ [] $ elemShow x
       span_ [] "."
     Nothing -> return ()
    when go $ do
      br_ []
      span_ [ "className" $= "line-start" ] $ "Please click "
      b_ [ "className" $= "new-game"
         , onClick $ \_ _ -> dispatch InitGame ] $ "here"
      span_ [] $ " for a new game."

renderMatrix :: AppState -> ReactElementM ViewEventHandler ()
renderMatrix state =
  case matrix state of
   Just (mat, cnt) -> do
     renderHeader_
     renderGame_ mat
     renderCounter_ cnt (gameOver state) (bestScore state)
   Nothing -> div_ [] (elemText "You should never be here!")


app :: ReactView ()
app = defineControllerView "app" store $ \state () -> renderMatrix state
  
main :: IO ()
main = do
  executeAction (SomeStoreAction store InitGame)
  reactRender "app" app ()

