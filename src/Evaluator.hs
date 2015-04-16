module Evaluator
(evaluateGame,
 evaluateMove,
 evaluateMoves)
where

import qualified Chess as C
import qualified Board as B

pieceTypeValue :: B.PieceType -> Int
pieceTypeValue B.Pawn = 1
pieceTypeValue B.Bishop = 3
pieceTypeValue B.Knight = 3
pieceTypeValue B.Rook = 5
pieceTypeValue B.Queen = 9
pieceTypeValue B.King = 0

colourModifier :: B.Colour -> Int
colourModifier B.White = 1
colourModifier B.Black = -1

pieceValue :: B.Piece -> Int
pieceValue (B.Piece t colour) = (pieceTypeValue t) * (colourModifier colour)

sumPieceValues :: [B.Piece] -> Int
sumPieceValues = (foldl (+) 0) . (map pieceValue)

evaluateGame :: C.Game -> Int
evaluateGame (C.Game (C.CheckMate B.White) _ _) = 1000
evaluateGame (C.Game (C.CheckMate B.Black) _ _) = -1000
evaluateGame (C.Game _ (currentBoard:boardHistory) moveHistory) = sumPieceValues $ B.getPieceList currentBoard

evaluateMove :: C.Game -> C.Move -> Int
evaluateMove game = evaluateGame . (C.makeMoveUnsafe game)

evaluateMoves :: C.Game -> [C.Move] -> [(C.Move, Int)]
evaluateMoves game moves = map (\m -> (m, evaluateMove game m)) moves
