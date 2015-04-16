-- AI for chess engine

module AI
(makeMove)
where

import qualified Chess as C
import qualified Board as B
import qualified Evaluator as E
import qualified Data.Ord as O
import qualified Data.List as L

type SearchDepth = Int

chooseFirstAvailableMove :: SearchDepth -> C.Game -> C.Move
chooseFirstAvailableMove _ = head . C.availableMoves

pickBestForColour :: B.Colour -> [(C.Move, Int)] -> C.Move
pickBestForColour B.White = fst . last . (L.sortBy (O.comparing snd))
pickBestForColour B.Black = fst. head . (L.sortBy (O.comparing snd))

bestScoreForColour :: B.Colour -> [Int] -> Int
bestScoreForColour B.White = last . L.sort
bestScoreForColour B.Black = head . L.sort

bestScore :: SearchDepth -> B.Colour -> C.Game -> C.Move -> Int
bestScore 0 _ game _ = E.evaluateGame game
bestScore n c game move
  | null nextMoves = E.evaluateGame updatedGame
  | otherwise = (bestScoreForColour c) $  map (bestScore (pred n) c updatedGame) nextMoves
  where updatedGame = C.makeMoveUnsafe game move
        nextMoves = C.availableMoves updatedGame -- FIXME short circuit if this is game over

-- need to do alpha beta pruning, otherwise AI is going to suck

minimax :: SearchDepth -> C.Game -> C.Move
minimax n game =
  pickBestForColour currentPlayer $ map (\m -> (m, bestScore n currentPlayer game m)) moves
    where moves = C.availableMoves game
          currentPlayer = C.currentPlayer game

--chooseMove = chooseFirstAvailableMove
chooseMove = minimax

makeMove:: SearchDepth -> C.Game -> Either C.MoveError C.Game
makeMove depth game = C.makeMove game (chooseMove depth game)
