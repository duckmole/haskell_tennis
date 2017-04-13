module Lib
    ( Player(..), Point(..), ScoreMatch(..), score, sequenceGame
    ) where

data Player = Player1 | Player2
            deriving (Show, Eq)
data Point = Love
           | Fifteen
           | Thirty
           deriving (Show, Eq)

data ScoreMatch = Normal Point Point
                | Forty Player Point
                | Deuce
                | Advantage Player
                | Game Player
                deriving (Show, Eq)

score :: ScoreMatch -> Player -> ScoreMatch
score Deuce winner = Advantage winner
score (Advantage player) winner
  | player == winner = Game winner
  | otherwise = Deuce
score (Forty player point) winner
  | player == winner = Game winner
  | point == Thirty = Deuce
  | otherwise = Forty player (increment point)
score (Normal Thirty point) Player1 =
  Forty Player1 point
score (Normal point Thirty) Player2 =
  Forty Player2 point
score (Normal point_player1 point_player2) Player1 =
  Normal (increment point_player1) point_player2
score (Normal point_player1 point_player2) Player2 =
  Normal point_player1 (increment point_player2)
score (Game winner) _ = Game winner

increment Love = Fifteen
increment Fifteen = Thirty

sequenceGame winners =
  foldl (\currentScore winner -> score currentScore winner) newGame winners
  where
    newGame = Normal Love Love
