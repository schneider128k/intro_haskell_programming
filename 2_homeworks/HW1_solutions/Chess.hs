-- J. Jakes-Schauer impl'd
-- If compilation fails because of syntax errors, or you see garbage characters here (Δ), for example, then you need better Unicode support!  This file uses UTF-8 encoding.

module Chess where
import Data.Char

-- See https://en.wikipedia.org/wiki/Chess for more details
-- Recall we only consider the situation where there is only a single
-- piece on the board

-- see Rules - Set up for basic definitions

type File     = Char         -- column index
                             -- valid files are 'a','b',...,'h'
type Rank     = Int          -- row index
                             -- valid ranks are 1,2,...8
type Position = (File,Rank)   

data Color =
  Black | White
  deriving (Eq,Show)

data Piece =
  King | Queen | Rook | Bishop | Knight | Pawn
  deriving (Eq,Show)

isLegalPosition :: Position -> Bool
isLegalPosition (f,r) = f `elem` ['a'..'h'] && r `elem` [1..8] -- Technically still O(1)

-- For taking the difference of two vectors:
posΔ :: Position -> Position -> (Int,Int)
posΔ (f1,r1) (f2,r2) = (ord f2 - ord f1, r2-r1)

-- Euclidean distance without taking the sqrt:
euclid² :: Position -> Position -> Int
euclid² p1 p2 =
  let (fΔ,rΔ) = posΔ p1 p2 in
   fΔ^2 + rΔ^2
  
-- White goes up the board, Black goes down it
isValidPawnMove color (f1,r1) (f2,r2) = f1 == f2 &&
                                        case color of
                                         White -> r1 == 2 && r2 == 4
                                                  || r2 == succ r1
                                         Black -> r1 == 7 && r2 == 5
                                                  || r2 == pred r1
                                         

-- The only perfect squares that add up to 5 are 1 and 4, so we can test a knight's move for validity based on the total displacement:
isValidKnightMove :: Position -> Position -> Bool
isValidKnightMove p1 p2 = euclid² p1 p2 == 5


isValidBishopMove :: Position -> Position -> Bool
isValidBishopMove p1 p2 = let (fΔ,rΔ) = posΔ p1 p2 in
                           abs fΔ == abs rΔ

isValidRookMove :: Position -> Position -> Bool
isValidRookMove (f1,r1) (f2,r2) = f1==f2 || r1==r2

isValidQueenMove :: Position -> Position -> Bool
isValidQueenMove p1 p2 = isValidRookMove p1 p2 || isValidBishopMove p1 p2

isValidKingMove p1 p2 = euclid² p1 p2 <= 2


isLegalMove :: Color -> Piece -> Position -> Position -> Bool
isLegalMove color piece p1 p2 =
  p1 /= p2
  && isLegalPosition p1
  && isLegalPosition p2
  && f p1 p2 where
    f = case piece of
      Pawn -> isValidPawnMove color 
      Knight -> isValidKnightMove 
      Bishop -> isValidBishopMove
      Rook -> isValidRookMove 
      Queen -> isValidQueenMove 
      King -> isValidKingMove 
      

