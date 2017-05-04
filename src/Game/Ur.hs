{-# LANGUAGE DeriveFunctor, RecordWildCards #-}

module Game.Ur where

import qualified Data.Vector as V

data UrBoard = UrBoard
  { lane :: Lanes
  , score :: Score
  } deriving Show

initialBoard :: UrBoard
initialBoard =
  UrBoard initialLanes initialScore

data Ur a
  = NoTurn a
  | AnotherTurn a
  | PlaceConflict a
  | PlaceSucceed a
  deriving (Show, Functor)

-- -- If this turns out to be useful, I'll uncomment it.
-- instance Applicative Ur where
--   pure = NoTurn

--   -- This follows the law -- pure id <*> v = v
--   NoTurn fn <*> ur =
--     case ur of
--       NoTurn a -> NoTurn (fn a)
--       AnotherTurn a -> AnotherTurn (fn a)
--       PlaceConflict a -> PlaceConflict (fn a)
--       PlaceSucceed a -> PlaceSucceed (fn a)

--   AnotherTurn fn <*> ur =
--     case ur of
--       NoTurn a -> AnotherTurn (fn a)
--       AnotherTurn a -> AnotherTurn (fn a)
--       PlaceConflict a -> AnotherTurn (fn a)
--       PlaceSucceed a -> AnotherTurn (fn a)

--   PlaceConflict fn <*> ur =
--     case ur of
--       NoTurn a -> PlaceConflict (fn a)
--       AnotherTurn a -> PlaceConflict (fn a)
--       PlaceConflict a -> PlaceConflict (fn a)
--       PlaceSucceed a -> PlaceConflict (fn a)

--   PlaceSucceed fn <*> ur =
--     case ur of
--       NoTurn a -> PlaceSucceed (fn a)
--       AnotherTurn a -> PlaceSucceed (fn a)
--       PlaceConflict a -> PlaceSucceed (fn a)
--       PlaceSucceed a -> PlaceSucceed (fn a)

-- instance Monad Ur where
--   NoTurn a >>= fn = fn a
--   AnotherTurn a >>= fn = fn a
--   PlaceConflict a >>= fn = fn a
--   PlaceSucceed a >>= fn = fn a

newtype Pos = Pos { unPos :: Int } deriving Show

data Lanes = Lanes
  { blackLane :: V.Vector Piece
  , whiteLane :: V.Vector Piece
  } deriving Show

initialLanes :: Lanes
initialLanes = let v = V.replicate 14 NoPiece in
  Lanes v v

getPlayerLane :: Turn -> Lanes -> V.Vector Piece
getPlayerLane BlackTurn = blackLane
getPlayerLane WhiteTurn = whiteLane

-- TODO: This function DEFINITELY needs to be property tested!
movePiece :: Turn -> Maybe Pos -> Int -> UrBoard -> Ur UrBoard

-- Putting a piece on the board doesn't have a previous position.
movePiece t Nothing i ur@UrBoard{..} =
  let dest    = i - 1
      pLane   = (getPlayerLane t) lane
      pAtDest = pLane V.!? dest
  in case (pAtDest, availablePieces t ur) of
    (           _, 0) -> PlaceConflict ur
    (Just NoPiece, _) ->
      if dest == 3
      then AnotherTurn (updateLanes t i) else PlaceSucceed (updateLanes t dest)
    _                 -> PlaceConflict ur
  where
    updateLanes BlackTurn u =
      ur { lane = lane { blackLane = V.update_ (blackLane lane) (V.singleton u) (V.singleton BlackPiece) } }
    updateLanes WhiteTurn u =
      ur { lane = lane { whiteLane = V.update_ (whiteLane lane) (V.singleton u) (V.singleton WhitePiece) } }

-- Moving a black piece further down the board.
movePiece BlackTurn (Just (Pos n)) i ur@UrBoard{..} =
  let dest     = n + i
      pLane    = blackLane lane
      pAtPos   = pLane V.!? n
      pAtDest  = pLane V.!? dest
      pAtOppo  = if dest > 3 && dest < 12 then whiteLane lane V.!? dest else Nothing
      update m = updateLanes [(n, NoPiece), (dest, BlackPiece)] m
  in case (pAtPos, pAtDest, pAtOppo) of
    (Just BlackPiece, Nothing, _) ->
      if dest == 14
      then PlaceSucceed
           (ur { lane  = lane  { blackLane = V.update_ (blackLane lane) (V.singleton n) (V.singleton NoPiece) }
               , score = score { blackScore = blackScore score + 1 }})
      else PlaceConflict ur
    (Just BlackPiece, Just NoPiece, Just WhitePiece) ->
      case dest of
        7 -> PlaceConflict ur
        _ -> PlaceSucceed (update (Just dest))
    (Just BlackPiece, Just NoPiece, _) ->
      if dest == 3 || dest == 7 || dest == 13
      then AnotherTurn (update Nothing) else PlaceSucceed (update Nothing)
    (_, _, _) ->
      PlaceConflict ur
   where
     updateLanes u Nothing  =
       ur { lane = lane { blackLane = V.update  (blackLane lane) (V.fromList  u) } }
     updateLanes u (Just v) =
       ur { lane = lane { blackLane = V.update  (blackLane lane) (V.fromList  u)
                        , whiteLane = V.update_ (whiteLane lane) (V.singleton v) (V.singleton NoPiece) } }

-- Moving a white piece further down the board.
movePiece WhiteTurn (Just (Pos n)) i ur@UrBoard{..} =
  let dest     = n + i
      pLane    = whiteLane lane
      pAtPos   = pLane V.!? n
      pAtDest  = pLane V.!? dest
      pAtOppo  = if dest > 3 && dest < 12 then blackLane lane V.!? dest else Nothing
      update m = updateLanes [(n, NoPiece), (dest, WhitePiece)] m
  in case (pAtPos, pAtDest, pAtOppo) of
    (Just WhitePiece, Nothing, _) ->
      if dest == 14
      then PlaceSucceed
           (ur { lane  = lane  { whiteLane = V.update_ (whiteLane lane) (V.singleton n) (V.singleton NoPiece) }
               , score = score { whiteScore = whiteScore score + 1 }})
      else PlaceConflict ur
    (Just WhitePiece, Just NoPiece, Just BlackPiece) ->
      case dest of
        7 -> PlaceConflict ur
        _ -> PlaceSucceed (update (Just dest))
    (Just WhitePiece, Just NoPiece, _) ->
      if dest == 3 || dest == 7 || dest == 13
      then AnotherTurn (update Nothing) else PlaceSucceed (update Nothing)
    (_, _, _) ->
      PlaceConflict ur
   where
     updateLanes u Nothing  =
       ur { lane = lane { whiteLane = V.update  (whiteLane lane) (V.fromList  u) } }
     updateLanes u (Just v) =
       ur { lane = lane { whiteLane = V.update  (whiteLane lane) (V.fromList  u)
                        , blackLane = V.update_ (blackLane lane) (V.singleton v) (V.singleton NoPiece) } }

data Turn
  = BlackTurn
  | WhiteTurn
  deriving (Show, Eq, Enum)

data Piece
  = NoPiece
  | BlackPiece
  | WhitePiece
  deriving (Show, Eq, Enum)

data Score = Score
  { blackScore :: Int
  , whiteScore :: Int
  } deriving Show

initialScore :: Score
initialScore = Score 0 0

availablePieces :: Turn -> UrBoard -> Int
availablePieces BlackTurn ur = 8 - (blackScore . score) ur - piecesOnBoard blackLane ur
availablePieces WhiteTurn ur = 8 - (whiteScore . score) ur - piecesOnBoard whiteLane ur

piecesOnBoard :: (Lanes -> V.Vector Piece) -> UrBoard -> Int
piecesOnBoard fn ur =
  foldr (\a b -> if a /= NoPiece then b + 1 else b) 0 (fn $ lane ur)

availableMoves :: Turn -> Int -> UrBoard -> V.Vector (Ur UrBoard)
availableMoves t i ur =
  V.filter noConflicts $ putOnBoard `V.cons` movePieces
  where
    putOnBoard = movePiece t Nothing i ur

    movePieces = foldr f (V.empty) (V.findIndices (== getPlayerTile t) $ getPlayerLane t $ lane ur)

    f n v = movePiece t (Just (Pos n)) i ur `V.cons` v

    getPlayerTile BlackTurn = BlackPiece
    getPlayerTile WhiteTurn = WhitePiece

    noConflicts PlaceConflict {} = False
    noConflicts _                = True