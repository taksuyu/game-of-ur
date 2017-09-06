{-# LANGUAGE DeriveFunctor, RecordWildCards #-}

module Game.Ur where

import qualified Data.Vector as V

data UrBoard = UrBoard
  { lane :: Lanes
  , score :: Score
  } deriving (Eq, Show)

initialBoard :: UrBoard
initialBoard =
  UrBoard initialLanes initialScore

-- FIXME: We can honestly replace this type with (Ur, a) but that's breaking so
-- It'll be done after a tag.
data Ur a
  = NoTurn a
  | AnotherTurn a
  | PlaceConflict a
  | PlaceSucceed a
  deriving (Show, Functor)

newtype Pos = Pos { unPos :: Int } deriving Show

data Lanes = Lanes
  { blackLane :: V.Vector Piece
  , whiteLane :: V.Vector Piece
  } deriving (Eq, Show)

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
      pLane   = getPlayerLane t lane
      pAtDest = pLane V.!? dest
  in case (pAtDest, availablePieces t ur) of
    (           _, 0) -> PlaceConflict ur
    (Just NoPiece, _) ->
      if dest == 3
      then AnotherTurn (updateLanes t dest) else PlaceSucceed (updateLanes t dest)
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
      update   = updateLanes [(n, NoPiece), (dest, BlackPiece)]
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
      update   = updateLanes [(n, NoPiece), (dest, WhitePiece)]
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

nextTurn :: Turn -> Turn
nextTurn t =
  case t of
    BlackTurn -> WhiteTurn
    WhiteTurn -> BlackTurn

data Piece
  = NoPiece
  | BlackPiece
  | WhitePiece
  deriving (Show, Eq, Enum)

hasPiece :: Piece -> Bool
hasPiece p = case p of
  BlackPiece -> True
  WhitePiece -> True
  _          -> False

data Score = Score
  { blackScore :: Int
  , whiteScore :: Int
  } deriving (Eq, Show)

initialScore :: Score
initialScore = Score 0 0

availablePieces :: Turn -> UrBoard -> Int
availablePieces BlackTurn ur = 7 - (blackScore . score) ur - piecesOnBoard blackLane ur
availablePieces WhiteTurn ur = 7 - (whiteScore . score) ur - piecesOnBoard whiteLane ur

piecesOnBoard :: (Lanes -> V.Vector Piece) -> UrBoard -> Int
piecesOnBoard fn ur =
  foldr (\a b -> if a /= NoPiece then b + 1 else b) 0 (fn $ lane ur)

availableMoves :: Turn -> Int -> UrBoard -> V.Vector (Ur UrBoard)
availableMoves t i ur =
  V.filter noConflicts $ putOnBoard `V.cons` movePieces
  where
    putOnBoard = movePiece t Nothing i ur

    movePieces = foldr f V.empty (V.findIndices (== getPlayerTile t) $ getPlayerLane t $ lane ur)

    f n v = movePiece t (Just (Pos n)) i ur `V.cons` v

    getPlayerTile BlackTurn = BlackPiece
    getPlayerTile WhiteTurn = WhitePiece

    noConflicts PlaceConflict {} = False
    noConflicts _                = True
