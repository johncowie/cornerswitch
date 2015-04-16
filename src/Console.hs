module Console
(printBoard) where

import qualified Board as B
import qualified Data.List as L
import qualified Chess as C
import qualified AI as A
import qualified Text.Read as R
import qualified Data.Tuple as Tuple

--  | a  | b  | c  | d  | e  | f  | g  | h  |
---------------------------------------------
--8 | bR | bN | bB | bQ | bK | bB | bN | nR | 8
--7 | bP | bP | bP | bP | bP | bP | bP | bP | 7
--6 |    |    |    |    |    |    |    |    | 6
--5 |    |    |    |    |    |    |    |    | 5
--4 |    |    |    |    |    |    |    |    | 4
--3 |    |    |    |    |    |    |    |    | 3
--2 | wP | wP | wP | wP | wP | wP | wP | wP | 2
--1 | wR | wN | wB | wQ | wK | wB | wN | wR | 1
---------------------------------------------
--  | a  | b  | c  | d  | e  | f  | g  | h  |

showColour :: B.Colour -> String
showColour B.White = "w"
showColour B.Black = "b"

showPieceType :: B.PieceType -> String
showPieceType B.Pawn = "P"
showPieceType B.Knight = "N"
showPieceType B.Bishop = "B"
showPieceType B.Rook = "R"
showPieceType B.Queen = "Q"
showPieceType B.King = "K"

showPiece :: B.Piece -> String
showPiece (B.Piece t c) = showColour c ++ showPieceType t

showSquare :: B.Board -> B.Rank -> B.File -> String
showSquare board r f = padTo4 $ maybe "  " showPiece (B.getPiece board (f, r))

padTo4 :: String -> String
padTo4 [c] = " " ++ [c] ++ "  "
padTo4 [c1, c2] = " " ++ [c1, c2] ++ " "

showFileRow = "  |" ++ (concat $ L.intersperse "|" $ map (\c -> padTo4 (c:[])) B.fileRange) ++ "|  \n"

showSeparatorRow = (take (8*4 + 9 + 6) $ repeat '-') ++ "\n"

showPieceRow :: B.Board -> B.Rank -> String
showPieceRow board rank = (show rank) ++ " |" ++ (concat $ L.intersperse "|" $ map (showSquare board rank) B.fileRange) ++ "| " ++ (show rank) ++ "\n"

showBoard :: B.Board -> String
showBoard board = showFileRow ++
                  showSeparatorRow ++
                  (concatMap (showPieceRow board) $ reverse B.rankRange) ++
                  showSeparatorRow ++
                  showFileRow

printBoard :: B.Board -> IO ()
printBoard = putStr . showBoard

printGame :: C.Game -> IO ()
printGame (C.Game _ (currentBoard:boardHistory) _) = printBoard currentBoard

readInt = R.readMaybe :: String -> Maybe Int

parseMove :: String -> Maybe C.Move
parseMove (a:b:c:d:xs) = case ((a, readInt (b:[])), (c, readInt (d:[]))) of
                          ((f1, Just r1), (f2, Just r2)) -> Just $ C.Move (f1, r1) (f2, r2)
                          _ -> Nothing
parseMove _ = Nothing

processHumanMove :: C.Game -> Maybe C.Move -> IO C.Game
processHumanMove game (Just move) =
  case C.makeMove game move of
    (Right updatedGame) -> return updatedGame
    (Left error) -> (human game)
processHumanMove game Nothing = human game

human :: C.Game -> IO C.Game
human game = do
  putStrLn $ "Enter a move: " ++ (show (C.currentPlayer game))
  moveInput <- getLine
  processHumanMove game (parseMove moveInput)

processComputerMove :: C.Game -> C.Game
processComputerMove game = case A.makeMove 3 game of
  (Right updatedGame) -> updatedGame
  (Left e) -> error "Computer shouldn't be able to make an invalid move."

computer :: C.Game -> IO C.Game
computer game = return $ processComputerMove game

nextMove :: ((C.Game -> IO C.Game), (C.Game -> IO C.Game)) -> C.Game -> IO ()
nextMove players game = do
  printGame game
  updatedGame <- (fst players) game
  let (C.Game status _ _) = updatedGame in case status of
    (C.CheckMate _) -> do
                    putStrLn "Check mate!"
                    printGame updatedGame
    C.StaleMate ->  do
                    putStrLn "Stale mate!"
                    printGame updatedGame
    otherwise -> nextMove (Tuple.swap players) updatedGame

startGame :: IO ()
startGame = nextMove (human, computer) C.newGame
