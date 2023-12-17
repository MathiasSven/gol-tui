{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -fplugin=Foreign.Storable.Generic.Plugin #-}
{-# OPTIONS_GHC -fplugin-opt=Foreign.Storable.Generic.Plugin:-v1 #-}

module Game
  ( Board(..)
  , BoardPtr
  , initializeBoard
  , flipBoardValue
  , setBoardValue
  , resizeBoard
  , stepBoard
  , renderBoard
  , clearBoard
  , cullBoring
  , randomizeBoard
  )
where

import           Data.Text                (Text)
import           Data.Text.Foreign        (fromPtr0)
import           Foreign
import           Foreign.C
import           Foreign.Storable.Generic (GStorable)
import           GHC.Generics             (Generic)
import qualified Language.C.Inline.Cpp    as C

data NdArray a
type BoardPtr = Ptr (NdArray CBool)

type Population = Int

data Board = MkBoard
  { _ptr        :: BoardPtr
  , _asText     :: Text
  , _steps      :: Int
  , _population :: Population
  }

data RenderResult = RenderResult (Ptr Word8) Word32
  deriving (Show, Generic)

instance GStorable RenderResult


C.context $ C.cppCtx <> C.cppTypePairs
  [ ("nc::NdArray", [t|NdArray|])
  , ("RenderResult", [t|RenderResult|])
  ]

C.include "game.hpp"

initializeBoard :: (CInt, CInt) -> IO (Ptr (NdArray CBool))
initializeBoard (rows, cols) = [C.exp| nc::NdArray<bool>* {
  new nc::NdArray<bool>(nc::zeros<bool>($(int rows), $(int cols)))
} |]

flipBoardValue :: (CInt, CInt) -> BoardPtr -> IO CBool
flipBoardValue (row, col) board = [C.block| bool {
  bool s = (*$(nc::NdArray<bool>* board))($(int row), $(int col));
  (*$(nc::NdArray<bool>* board))($(int row), $(int col)) = not s;
  return not s;
} |]

setBoardValue :: (CInt, CInt) -> CBool -> BoardPtr -> IO ()
setBoardValue (row, col) val board = [C.block| void {
  (*$(nc::NdArray<bool>* board))($(int row), $(int col)) = $(bool val);
} |]

resizeBoard :: (CInt, CInt) -> BoardPtr -> IO ()
resizeBoard (rows, cols) board =
  [C.exp| void {
    resize_board($(nc::NdArray<bool>* board), $(int rows), $(int cols))
  } |]

stepBoard :: BoardPtr -> IO ()
stepBoard board =
  [C.exp| void { step_board($(nc::NdArray<bool>* board)) } |]

renderBoard :: BoardPtr -> IO (Text, Int)
renderBoard board = do
  rptr <- [C.exp| RenderResult* { render_board($(nc::NdArray<bool>* board)) } |]
  RenderResult rtext rpop <- peek rptr
  (,fromIntegral rpop) <$> fromPtr0 rtext <* free rtext <* free rptr

clearBoard :: BoardPtr -> IO ()
clearBoard board =
  [C.exp| void { clear_board($(nc::NdArray<bool>* board)) } |]

cullBoring :: BoardPtr -> IO ()
cullBoring board =
  [C.exp| void { cull_boring_patterns($(nc::NdArray<bool>* board)) } |]

randomizeBoard :: BoardPtr -> IO ()
randomizeBoard board =
  [C.exp| void { randomize_board($(nc::NdArray<bool>* board)) } |]
