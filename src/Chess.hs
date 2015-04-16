-- Design thoughts TODO
--  should move have captureFlag within it? Probably not
--  game needs to have move history stored, as well as previous board positions .i.e moveHistory, boardHistory
-- should current board position be separate from board history? -- prolly not
-- should board and move history be combined to ensure coupling between move and board?
-- pass around hasPieceMoved? function by closing over moveHistory and checking origin of each move

module Chess
(newGame,
 makeMove,
 makeMoveUnsafe,
 availableMoves,
 currentPlayer,
 Game(..),
 GameStatus(..),
 MoveError(..),
 Move(..)
) where

import qualified Board as B
import qualified Data.Maybe as Maybe

type Direction = (Int, Int)
data GameStatus = InProgress | Check B.Colour | CheckMate B.Colour | StaleMate deriving (Show, Eq)
data Move = Move B.Square B.Square deriving (Show, Eq)
data MoveError = InvalidMove B.Square B.Square | GameFinished deriving (Show)
data Game = Game GameStatus BoardHistory MoveHistory deriving (Show)
type BoardHistory = [B.Board]
type MoveHistory = [(Move, B.Piece)]

captureMove orig dest = (Move orig dest, True)
nonCaptureMove orig dest = (Move orig dest, False)
isCaptureMove = snd
moveDest (Move _ dest) = dest

startingPawns rank colour = map (\f -> ((f, rank), B.pawn colour)) ['a'..'h']
startingRooks rank colour = [(('a', rank), B.rook colour), (('h', rank), B.rook colour)]
startingKnights rank colour = [(('b', rank), B.knight colour), (('g', rank), B.knight colour)]
startingBishops rank colour = [(('c', rank), B.bishop colour), (('f', rank), B.bishop colour)]
startingQueen rank colour = [(('d', rank), B.queen colour)]
startingKing rank colour = [(('e', rank), B.king colour)]
startingPieces frontRank backRank colour =
        startingPawns frontRank colour ++
        startingRooks backRank colour ++
        startingKnights backRank colour ++
        startingBishops backRank colour ++
        startingKing backRank colour ++
        startingQueen backRank colour

startingPositions = startingPieces 2 1 B.White ++ startingPieces 7 8 B.Black

startingBoard :: B.Board
startingBoard = foldl B.addPieceToBoard B.emptyBoard startingPositions

isOccupiedByOpponent :: B.Board -> B.Colour -> B.Square -> Bool
isOccupiedByOpponent board colour square = case B.getPiece board square of
                                              Nothing -> False
                                              (Just (B.Piece _ occupantColour)) -> colour /= occupantColour

nInc :: (Enum a) => a -> Int -> a
nInc x 0 = x
nInc x i = if (i > 0) then nInc (succ x) (pred i) else nInc (pred x) (succ i)

translateSquare :: B.Square -> Direction -> B.Square
translateSquare (f, r) (x, y) =  ((nInc f x), (nInc r y))

availableSquaresInDirection :: B.Board -> B.Colour -> B.Square -> Direction -> [(B.Square, Bool)]
availableSquaresInDirection board colour square dir
  | B.isSquareValid newSquare && B.isEmptySquare board newSquare = (newSquare, False):(availableSquaresInDirection board colour newSquare dir)
  | B.isSquareValid newSquare && isOccupiedByOpponent board colour newSquare = [(newSquare, True)]
  | otherwise = []
  where newSquare = translateSquare square dir

availableMovesInDirection :: B.Board -> B.Colour -> B.Square -> Direction -> [(Move, Bool)]
availableMovesInDirection board colour orig dir =
  map (\(dest, isCapture) -> ((Move orig dest), isCapture)) $ availableSquaresInDirection board colour orig dir

straightDirections = [(1, 0), (0, 1), (-1, 0), (0, -1)]
diagonalDirections = [(1, 1), (1, -1), (-1, -1), (-1, 1)]
knightDirections = [(2, 1), (1, 2), (-1, 2), (-2, 1), (-2, -1), (-1, -2), (1, -2), (2, -1)]

tryNonCaptureMove :: B.Board -> B.Colour -> B.Square -> B.Square -> Maybe (Move, Bool)
tryNonCaptureMove board colour orig dest =
  if B.isSquareValid dest && B.isEmptySquare board dest then Just $ nonCaptureMove orig dest else Nothing

tryCaptureMove :: B.Board -> B.Colour -> B.Square -> B.Square -> Maybe (Move, Bool)
tryCaptureMove board colour orig dest =
  if B.isSquareValid dest && isOccupiedByOpponent board colour dest then Just $ captureMove orig dest else Nothing

generateMove :: B.Board -> B.Colour -> B.Square -> B.Square -> Maybe (Move, Bool)
generateMove board colour orig dest
  | B.isSquareValid dest && B.isEmptySquare board dest = Just $ nonCaptureMove orig dest
  | B.isSquareValid dest && isOccupiedByOpponent board colour dest = Just $ captureMove orig dest
  | otherwise = Nothing
  -- FIXME rewrite to use functions above

  -- Pawn Moves
  -- One forward - Done
  -- One diagonally left or right if taking - Done
  -- TODO Two forward or one forward if on start square - Not Done
  -- TODO queening the pawn - Not Done
  -- TODO En passant - Not Done
pawnMoves :: B.Board -> B.Colour -> B.Square -> [(Move, Bool)]
pawnMoves board colour orig = forwardMoves ++ takingMoves
  where forwardMoves =  Maybe.mapMaybe (tryNonCaptureMove board colour orig) [translateSquare orig (0, colourDir)]
        takingMoves = Maybe.mapMaybe (tryCaptureMove board colour orig) $ map (translateSquare orig) [(-1, colourDir), (1, colourDir)]
        colourDir = if colour == B.White then 1 else -1

availablePieceMoves :: B.Board -> B.Piece -> B.Square -> [(Move, Bool)]
availablePieceMoves board (B.Piece B.Pawn colour) square = pawnMoves board colour square

availablePieceMoves board (B.Piece B.Bishop colour) square =
  concatMap (availableMovesInDirection board colour square) diagonalDirections

availablePieceMoves board (B.Piece B.Knight colour) orig =
  Maybe.mapMaybe (generateMove board colour orig) $ map (translateSquare orig) knightDirections

availablePieceMoves board (B.Piece B.Queen colour) square =
  concatMap (availableMovesInDirection board colour square) (straightDirections ++ diagonalDirections)

availablePieceMoves board (B.Piece B.Rook colour) square =
  concatMap (availableMovesInDirection board colour square) straightDirections

-- TODO look for castling possibilities - need to store if king has moved or not
availablePieceMoves board (B.Piece B.King colour) orig =
  Maybe.mapMaybe (generateMove board colour orig) $ map (translateSquare orig) (straightDirections ++ diagonalDirections)

findKing :: B.Board -> B.Colour -> B.Square
findKing board colour = case B.findMatchingPieces board (B.Piece B.King colour) of
                            [kingSquare] -> kingSquare
                            _ -> error "There should only be one king"

isInCheck :: B.Board -> B.Colour  -> Bool
isInCheck board colour = elem (findKing board colour) (attackedSquares board (B.oppositeColour colour))

isMoveIntoCheck :: B.Board -> B.Colour -> Move -> Bool
isMoveIntoCheck board colour (Move orig dest) = isInCheck (B.movePiece board orig dest) colour

availableMovesIgnoreCheck :: B.Board -> B.Colour -> [(Move, Bool)]
availableMovesIgnoreCheck board colour = concatMap (\(square, piece) -> availablePieceMoves board piece square) $ B.getPieces board colour

attackedSquares :: B.Board -> B.Colour -> [B.Square]
attackedSquares board colour = map moveDest $ map fst $ filter snd $ availableMovesIgnoreCheck board colour

availableMoves :: Game -> [Move]
availableMoves (Game _ boardHistory moveHistory)
  = filter (not . isMoveIntoCheck board colour) $ map fst $ availableMovesIgnoreCheck board colour
  where board = currentBoard boardHistory
        colour = playerTurn moveHistory

---- GAME STUFF ----
newGame :: Game
newGame = Game InProgress [startingBoard] []

makeValidMove :: B.Square -> B.Square -> Game -> Game
makeValidMove orig dest (Game status boardHistory moveHistory)
  = updateGameStatus $ Game status ((B.movePiece board orig dest):boardHistory) ((Move orig dest, piece):moveHistory)
  where (Just piece) = B.getPiece board orig
        board = currentBoard boardHistory

currentBoard :: BoardHistory -> B.Board
currentBoard = head

currentPlayer :: Game -> B.Colour
currentPlayer (Game _ _ moveHistory) = playerTurn moveHistory

playerTurn :: MoveHistory -> B.Colour
playerTurn [] = B.White
playerTurn moveHistory = B.oppositeColour . colour . snd . head $ moveHistory
  where colour (B.Piece _ c) = c

makeMove :: Game -> Move -> Either MoveError Game
makeMove game@(Game status boardHistory moveHistory) move@(Move orig dest)
  | status /= InProgress = Left GameFinished
  | elem move validMoves = Right $ makeValidMove orig dest game
	| otherwise = Left (InvalidMove orig dest)
	where validMoves = availableMoves game

makeMoveUnsafe :: Game -> Move -> Game
makeMoveUnsafe game move = case makeMove game move of
                            (Right updatedGame) -> updatedGame
                            (Left e) -> error $ (show e) ++ (show move)

-- TODO 50 moves without capture = tie
-- TODO 3 repeated board positions = tie
-- TODO insufficientMaterial = tie
updateGameStatus :: Game -> Game
updateGameStatus game@(Game status boardHistory moveHistory)
   | checked && null moves = setGameStatus $ CheckMate opponent
   | null moves = setGameStatus StaleMate
   | checked = setGameStatus $ Check opponent
   | otherwise = setGameStatus InProgress
   where moves = availableMoves game
         checked = isInCheck current player
         current = currentBoard boardHistory
         player = playerTurn moveHistory
         opponent = B.oppositeColour player
         setGameStatus status = Game status boardHistory moveHistory
