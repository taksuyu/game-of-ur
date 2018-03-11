{-# LANGUAGE LambdaCase, NamedFieldPuns #-}

module Game.Ur where

import qualified Data.IntSet as IS

import Control.Applicative
import Control.Monad
import Data.Maybe
import System.Random
  ( getStdRandom, randomR )

data Board = Board
  { black :: Side
  , white :: Side
  , dice :: Int
  , turn :: Turn
  } deriving (Eq, Show)

initialBoard :: Int -> Turn -> Board
initialBoard dice turn = Board initialSide initialSide dice turn

newBoard :: IO Board
newBoard = pure initialBoard <*> newDiceRoll <*> fmap ([Black, White] !! ) (getStdRandom $ randomR (0, 1))

-- | A side represents a players total pieces, the current positions of pieces
-- on the board, and how many pieces have been scored.
data Side = Side
  { pieces :: Int
    -- ^ The amount of pieces a player can place on the board
  , path :: IS.IntSet
    -- ^ The positions of the pieces currently on the board
  , scored :: Int
    -- ^ How many pieces have made it through the board
  } deriving (Eq, Show)

initialSide :: Side
initialSide = Side 7 IS.empty 0

-- | A return type to tell what the next game state should be.
data Ur
  = AnotherTurn
  | PlaceSucceed
  deriving Show

data Turn
  = White
  | Black
  deriving (Show, Eq, Ord)

nextTurn :: Turn -> Turn
nextTurn = \case
  Black -> White
  White -> Black

-- getPlayerLane :: Turn -> Lanes -> V.Vector Piece
-- getPlayerLane BlackTurn = blackLane
-- getPlayerLane WhiteTurn = whiteLane

nextBoardState :: (Ur, Board) -> IO Board
nextBoardState ur = do
  newDice <- newDiceRoll
  pure $ nextBoard newDice ur

nextBoard :: Int -> (Ur, Board) -> Board
nextBoard newDice = \case
  (AnotherTurn, board) -> do
    board { dice = newDice }
  (PlaceSucceed, board@Board{ turn }) -> do
    board { dice = newDice, turn = nextTurn turn }

-- | Helper function for generating dice rolls.
newDiceRoll :: IO Int
newDiceRoll = fmap sum . replicateM 4 $ getStdRandom (randomR (0, 1))

move :: Int -> Board -> Maybe (Ur, Board)
move i b@Board{ black = black@Side{ path = blackPath }, white = white@Side{ path = whitePath }, dice, turn } =
  case turn of
    Black -> findMove black
    White -> findMove white

  where
    findMove side = placePiece side <|> movePiece side <|> takePiece side

    placePiece side@Side{ pieces, path } =
      case (i == dice && not (IS.member i path), turn) of
        (True, Black) ->
          if pieces > 0
          then if i == 4
            then
              Just $ (AnotherTurn, b { black = side { pieces = pieces - 1, path = IS.insert i path } })
            else
              Just $ (PlaceSucceed, b { black = side { pieces = pieces - 1, path = IS.insert i path } })
          else Nothing
        (True, White) ->
          if pieces > 0
          then if i == 4
            then
              Just $ (AnotherTurn, b { white = side { pieces = pieces - 1, path = IS.insert i path } })
            else
              Just $ (PlaceSucceed, b { white = side { pieces = pieces - 1, path = IS.insert i path } })
          else Nothing
        _ ->
          Nothing

    movePiece side@Side{ path } =
      case (IS.member i path && i + dice < 15 && not (IS.member (i + dice) path), turn) of
        (True, Black) ->
          if not (i + dice == 8 && IS.member 8 whitePath)
          then case (i + dice > 4 && i + dice < 13, IS.member (i + dice) whitePath) of
            (True, True) ->
              Just $ (PlaceSucceed, b { black = side { path = IS.insert (i + dice) $ IS.delete i path }
                                      , white = white { path = IS.delete (i + dice) whitePath
                                                      , pieces = pieces white + 1 }
                                      })
            (True, False) ->
              if i + dice == 8
              then Just $ (AnotherTurn, b { black = side { path = IS.insert (i + dice) $ IS.delete i path } })
              else Just $ (PlaceSucceed, b { black = side { path = IS.insert (i + dice) $ IS.delete i path } })
            _ ->
              if i + dice == 4 || i + dice == 14
              then Just $ (AnotherTurn, b { black = side { path = IS.insert (i + dice) $ IS.delete i path } })
              else Just $ (PlaceSucceed, b { black = side { path = IS.insert (i + dice) $ IS.delete i path } })
          else Nothing
        (True, White) ->
          if not (i + dice == 8 && IS.member 8 blackPath)
          then case (i + dice > 4 && i + dice < 13, IS.member (i + dice) blackPath) of
            (True, True) ->
              Just $ (PlaceSucceed, b { white = side { path = IS.insert (i + dice) $ IS.delete i path }
                                      , black = black { path = IS.delete (i + dice) blackPath
                                                      , pieces = pieces black + 1 }
                                      })
            (True, False) ->
              if i + dice == 8
              then Just $ (AnotherTurn, b { white = side { path = IS.insert (i + dice) $ IS.delete i path } })
              else Just $ (PlaceSucceed, b { white = side { path = IS.insert (i + dice) $ IS.delete i path } })
            _ ->
              if i + dice == 4 || i + dice == 14
              then Just $ (AnotherTurn, b { white = side { path = IS.insert (i + dice) $ IS.delete i path } })
              else Just $ (PlaceSucceed, b { white = side { path = IS.insert (i + dice) $ IS.delete i path } })
          else Nothing
        _ ->
          Nothing

    takePiece side@Side{ path, scored } =
      case (IS.member i path && i + dice == 15, turn) of
        (True, Black) ->
          Just $ (PlaceSucceed, b { black = side { path = IS.delete i path, scored = scored + 1 } })
        (True, White) ->
          Just $ (PlaceSucceed, b { white = side { path = IS.delete i path, scored = scored + 1 } })
        _ ->
          Nothing

piecesOnBoard :: Board -> Int
piecesOnBoard Board{ black, white, turn } =
  case turn of
    Black -> IS.size (path black)
    White -> IS.size (path white)

availableMoves :: Board -> [(Ur, Board)]
availableMoves board =
  mapMaybe (`move` board) [1..14]
