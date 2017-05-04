{-# LANGUAGE LambdaCase, TemplateHaskell #-}

module Test.Game.Ur where

import Hedgehog
import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range

import qualified Data.Vector    as V

import Game.Ur

-- TODO: Moves do take pieces when they aren't safe
-- TODO: Moves don't take pieces when they are safe

applyMoves :: [(Turn, Int, Int)] -> UrBoard
applyMoves = foldl f initialBoard
  where
    f ur (t, n, p) = let moves' = availableMoves t n ur in
      case V.toList moves' of
        [] -> ur
        l  -> processMove (l !! (mod p (length l)))

moves :: Monad m => Gen m [(Turn, Int, Int)]
moves = Gen.list (Range.linear 0 500) $ do
  turn <- Gen.enum BlackTurn WhiteTurn
  num  <- Gen.int (Range.linear 1 4) -- We want to ignore rolls of 0 as they just get skipped
  nump <- Gen.int (Range.linear 0 7)
  pure (turn, num, nump)

-- We want to ignore invalid moves that are generated.
processMove :: Ur UrBoard -> UrBoard
processMove = \case
  PlaceSucceed b  -> b
  AnotherTurn  b  -> b

  -- Not used, but I want the function to be total
  NoTurn a -> a
  PlaceConflict a -> a

-- Properties --

prop_gamescore_max :: Property
prop_gamescore_max = property $ do
  m <- forAll moves
  let (UrBoard _ (Score bs ws)) = applyMoves m
  assert $ bs <= 8
  assert $ ws <= 8

prop_pieces_on_board :: Property
prop_pieces_on_board = property $ do
  m <- forAll moves
  let ur = applyMoves m
  assert $ piecesOnBoard blackLane ur <= 8
  assert $ piecesOnBoard whiteLane ur <= 8

tests :: IO Bool
tests =
  checkParallel $$(discover)