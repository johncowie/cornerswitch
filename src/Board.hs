module Board
(Colour(..),
 Square,
 PieceType(..),
 Piece(..),
 Board,
 Rank,
 File,
 movePiece,
 findMatchingPieces,
 addPieceToBoard,
 emptyBoard,
 isEmptySquare,
 getPieces,
 getPieceList,
 getPiece,
 oppositeColour,
 isSquareValid,
 pawn,
 knight,
 bishop,
 rook,
 queen,
 king,
 rankRange,
 fileRange) where

import qualified Data.Map as M

type Rank = Int
type File = Char
type Square = (File, Rank)
type Move = (Square, Square)
data Colour = Black | White deriving (Show, Eq)
data PieceType = Pawn | Knight | Bishop | Rook | Queen | King deriving (Show, Eq)
data Piece = 	Piece PieceType Colour deriving (Show, Eq)
type Board = M.Map Square Piece

pawn = Piece Pawn
knight = Piece Knight
bishop = Piece Bishop
rook = Piece Rook
queen = Piece Queen
king = Piece King

emptyBoard :: Board
emptyBoard = M.empty

addPieceToBoard :: Board -> (Square, Piece) -> Board
addPieceToBoard board (square, piece) = M.insert square piece board

getPiece :: Board -> Square -> Maybe Piece
getPiece board square = M.lookup square board

getPieces :: Board -> Colour -> [(Square, Piece)]
getPieces board colour = filter (\(_ , (Piece _ c)) -> c == colour) $ M.toList board

getPieceList :: Board -> [Piece]
getPieceList = (map snd) . M.toList

movePiece :: Board -> Square -> Square -> Board
movePiece board from to = case getPiece board from of
                          Nothing -> board
                          (Just piece) -> (M.insert to piece) . (M.delete from) $ board

findMatchingPieces :: Board -> Piece -> [Square]
findMatchingPieces board piece = map fst $ filter (\(_, p)  -> p == piece) (M.toList board)

isRankValid :: Rank -> Bool
isRankValid rank = rank >= 1 && rank <= 8

isFileValid :: File -> Bool
isFileValid file = file >= 'a' && file <= 'h'

rankRange = [1 .. 8] :: [Rank]
fileRange = ['a' .. 'h'] :: [File]

isSquareValid :: Square -> Bool
isSquareValid (file, rank) = isRankValid rank && isFileValid file

isEmptySquare :: Board -> Square -> Bool
isEmptySquare board square = getPiece board square == Nothing

oppositeColour :: Colour -> Colour
oppositeColour White = Black
oppositeColour Black = White
