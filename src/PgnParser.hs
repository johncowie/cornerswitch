-- http://en.wikipedia.org/wiki/Portable_Game_Notation
import qualified Chess as C

data ParseError = InvalidStringError | InvalidMoveError

parseMove :: C.Board -> String -> Either ParseError C.Move
parseMove board string = (error "unimplemented")
