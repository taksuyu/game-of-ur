{-# LANGUAGE KindSignatures, LambdaCase, NamedFieldPuns, RecordWildCards,
  TemplateHaskell, TupleSections #-}

module Test.Game.Ur where

import Hedgehog
import qualified Hedgehog.Gen               as Gen
import qualified Hedgehog.Internal.Property as HIP
import qualified Hedgehog.Range             as Range
import qualified Data.IntSet                as IS
import qualified Data.List                  as L

import Control.Monad
import Control.Monad.IO.Class
import Data.Foldable
import Data.Maybe
import Data.IORef
import Game.Ur

-- TODO: Moves do take pieces when they aren't safe
-- TODO: Moves don't take pieces when they are safe

applyMoves :: [Int] -> IO Board
applyMoves ns = do
  board <- newBoard
  foldlM discardPass board ns

discardPass :: Board -> Int -> IO Board
discardPass board i =
  case availableMoves board of
    [] -> nextBoardState (PlaceSucceed, board)
    l  -> nextBoardState (l !! mod i (length l))

moves :: Gen [(Int)]
moves = Gen.list (Range.linear 0 5000) $ do
  nump <- Gen.int (Range.linear 0 7)
  pure nump

boards :: GenT IO [Board]
boards = Gen.list (Range.linear 0 500) $ do
  board <- liftIO newBoard
  pure board

-- Properties --

-- NOTE: This could generate boards over nextBoardState to properly show boards
-- on a failure.
prop_diceroll_never_over_four :: Property
prop_diceroll_never_over_four = property $ do
  m <- HIP.forAllT boards
  foldlM (\ _ Board{ dice } -> assert $ dice <= 4) () m

-- NOTE: This could generate boards over nextBoardState to properly show boards
-- on a failure.
prop_diceroll_never_below_zero :: Property
prop_diceroll_never_below_zero = property $ do
  m <- HIP.forAllT boards
  foldlM (\ _ Board{ dice } -> assert $ dice >= 0) () m

prop_gamescore_max :: Property
prop_gamescore_max = property $ do
  m <- forAll moves
  Board{ black = Side{ scored = bs }, white = Side{ scored = ws }} <- evalIO $ applyMoves m
  assert $ bs <= 7
  assert $ ws <= 7

prop_pieces_on_board :: Property
prop_pieces_on_board = property $ do
  m <- forAll moves
  board <- evalIO $ applyMoves m
  assert $ piecesOnBoard board <= 7

newtype UrBoard = UrBoard { unUrBoard :: IORef Board }

newUrBoard :: IO UrBoard
newUrBoard = UrBoard <$> join (newIORef <$> newBoard)

placePiece :: UrBoard -> IO Bool
placePiece (UrBoard ref) = do
  newDice <- newDiceRoll
  atomicModifyIORef' ref $ \b@Board{ dice } ->
    case move dice b of
      Just ur ->
        (nextBoard newDice ur, True)
      Nothing ->
        (b, False)

data ModelState (v :: * -> *) = ModelState
  { boardState :: [(Int, Turn)]
  , diceState :: Int
  , turnState :: Turn
  }

initialModelState :: Int -> Turn -> ModelState v
initialModelState = ModelState []

data Place (v :: * -> *)
  = Place deriving (Show, Eq)

instance HTraversable Place where
  htraverse _ _ = pure Place

s_place :: (Monad n, MonadIO m, MonadTest m) => UrBoard -> Command n m ModelState
s_place urb =
  let gen ModelState{ boardState, diceState, turnState } =
        if diceState > 0 && not (elem (diceState, turnState) boardState)
        then
          Just $ pure Place
        else
          Nothing

      execute Place =
        liftIO $ placePiece urb

  in Command gen execute
     [ Update $ \s@ModelState{ boardState, diceState, turnState } Place _o ->
         s { boardState = L.insert (diceState, turnState) $ boardState }
     , Ensure $ \ModelState{ diceState, turnState } ModelState{ boardState } Place _ ->
         assert $ length (filter (/= (diceState, turnState)) boardState) == 1
     ]

data Move = Move deriving (Show, Eq)

data Take = Take deriving (Show, Eq)

data Pass = Pass deriving (Show, Eq)

-- TODO: Support in hedgehog needs to provide a way for us to test that there
-- exists a failure as intended.
--
-- prop_cant_always_move_pieces :: Property
-- prop_cant_always_move_pieces = property $ do
--   m <- forAll moves
--   _ <- applyMoves' m
--   pure ()
--   where
--   applyMoves' :: [(Turn, Int, Int)] -> Test IO UrBoard
--   applyMoves' = flip foldl (pure initialBoard) $
--     \ ur (t, n, p) -> do
--       board <- ur
--       let moves' = availableMoves t n board in
--         case V.toList moves' of
--         [] -> do
--           failure
--         l  -> do
--           pure $ processMove (l !! (mod p (length l)))

prop_no_middle_lane_overlap :: Property
prop_no_middle_lane_overlap = property $ do
  m <- forAll moves
  Board{ black, white } <- evalIO $ applyMoves m
  assert
    $ flip all [5..12]
    $ \ n -> not $ IS.member n (path black) && IS.member n (path white)

prop_cant_take_eighth_spot_piece_from_other_player :: Property
prop_cant_take_eighth_spot_piece_from_other_player = property $ do
  m <- forAll moves
  board <- evalIO newBoard
  _ <- foldlM (\Board{ black = Side{ path = blackPath }, white = Side{ path = whitePath } } i -> do
            let spot8 path turn = if IS.member 8 path then Just turn else Nothing
            let blackSpot8 = spot8 blackPath Black
            let whiteSpot8 = spot8 whitePath White
            assert $ not $ isJust blackSpot8 && isJust whiteSpot8

            b@Board{ black = Side{ path = newBlackPath }, white = Side{ path = newWhitePath } } <- evalIO $ discardPass board i
            let newBlackSpot8 = spot8 newBlackPath Black
            let newWhiteSpot8 = spot8 newWhitePath White
            assert $ not (isJust blackSpot8 && isJust newWhiteSpot8)
              && not (isJust whiteSpot8 && isJust newBlackSpot8)
            pure b
            ) board m
  pure ()

-- prop_cant_place_piece_over_existing_pieces :: Property
-- prop_cant_place_piece_over_existing_pieces = property $ do
--   m <- forAll moves
--   board <- evalIO newBoard
--   _ <- foldlM (\Board{ black = Side{ path = blackPath }, white = Side{ path = whitePath }, turn } i ->
--                   (\ board' i' -> do
--                      (pass, b@Board{ black = Side{ path = newBlackPath }, white = Side{ path = newWhitePath } }) <- evalIO $ case availableMoves board' of
--                       [] -> (True,) <$> nextBoardState (PlaceSucceed, board')
--                       l  -> (False,) <$>  nextBoardState (l !! mod i' (length l))

--                      if not pass
--                        then
--                        case turn of
--                          Black ->
--                            assert $ blackPath /= newBlackPath
--                          White ->
--                            assert $ whitePath /= newWhitePath
--                        else
--                        pure ()

--                      pure b
--                     ) board i
--               ) board m
--   pure ()

-- data Side = Side
--   { piecesAvailable :: Int
--   , path :: IS.IntSet
--   , scoredPieces :: Int
--   }

-- newSide :: Side
-- newSide = Side 7 IS.empty 0

-- data Board = Board
--   { sideBlack :: IORef Side
--   , sideWhite :: IORef Side
--   , pTurn :: IORef Turn
--   }

-- newBoard :: IO Board
-- newBoard = pure Board <*> newIORef newSide <*> newIORef newSide <*> newIORef BlackTurn

-- placePieceOnBoard :: Int -> Board -> IO ()
-- placePieceOnBoard dice Board{ sideBlack, sideWhite, pTurn } = do
--   turn <- readIORef pTurn
--   let placePieceSide playerSide = atomicModifyIORef' playerSide
--         $ \side@Side { piecesAvailable, path } ->
--             ( side { piecesAvailable = piecesAvailable - 1
--                    , path = IS.insert dice path
--                    }
--             , ())
--   case turn of
--     BlackTurn ->
--       placePieceSide sideBlack
--     WhiteTurn ->
--       placePieceSide sideWhite

-- movePieceOnBoard :: Pos -> Int -> Board -> IO ()
-- movePieceOnBoard (Pos n) dice Board{ sideBlack, sideWhite, pTurn } = do
--   turn <- readIORef pTurn
--   let movePieceSide playerSide otherSidePath = atomicModifyIORef' playerSide
--         $ \side@Side{ path } ->
--             if n + dice > 4 && n + dice < 13
--             then
--               case ( IS.member n path && not (IS.member (n + dice) path)
--                    , not $ IS.member (n + dice) otherSidePath && n + dice == 8
--                    , IS.member (n + dice) otherSidePath )
--               of
--                 (True, True, False) ->
--                   ( side { path = IS.insert (n + dice) $ IS.delete n path }
--                   , False )
--                 (True, True, True) ->
--                   ( side { path = IS.insert (n + dice) $ IS.delete n path }
--                   , True )
--                 _ ->
--                   ( side, False )
--             else
--               if IS.member n path && not (IS.member (n + dice) path)
--               then
--                 ( side { path = IS.insert (n + dice) $ IS.delete n path }
--                 , False )
--               else
--                 ( side, False )
--   case turn of
--     BlackTurn -> do
--       moreWork <- movePieceSide sideBlack . path =<< readIORef sideBlack
--       if moreWork
--         then atomicModifyIORef' sideWhite
--              $ \side@Side{ path } ->
--                  ( side { path = IS.delete (n + dice) path }
--                  , () )
--         else pure ()
--     WhiteTurn -> do
--       moreWork <- movePieceSide sideWhite . path =<< readIORef sideBlack
--       if moreWork
--         then atomicModifyIORef' sideBlack
--              $ \side@Side{ path } ->
--                  ( side { path = IS.delete (n + dice) path }
--                  , () )
--         else pure ()

-- takePieceOffBoard :: Pos -> Int -> Board -> IO ()
-- takePieceOffBoard (Pos n) dice Board{ sideBlack, sideWhite, pTurn } = do
--   turn <- readIORef pTurn
--   let takePieceSide playerSide = atomicModifyIORef' playerSide $
--         \side@Side{ path, scoredPieces } ->
--           if IS.member n path && n + dice == 15
--           then
--             ( side { path = IS.delete n path, scoredPieces = scoredPieces + 1 }
--             , () )
--           else
--             ( side, () )
--   case turn of
--     BlackTurn ->
--       takePieceSide sideBlack
--     WhiteTurn ->
--       takePieceSide sideWhite

tests :: IO Bool
tests = checkParallel $$(discover)
