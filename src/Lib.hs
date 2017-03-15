module Lib (Player(..), Score(..), Point(..), FortyData(..), score) where

data Player = Player1 | Player2
            deriving (Show, Eq)

data Point = Love
           | Fifteen
           | Thirty
           deriving (Show, Eq)

data PointsData = PointsData { playerOnePoint :: Point
                             , playerTwoPoint :: Point
                             }
                  deriving (Show, Eq)
data FortyData = FortyData { player :: Player,
                             otherPlayerPoint :: Point
                           }
                 deriving (Show, Eq)

data Score = Points PointsData
  | Forty FortyData
  | Deuce
  | Advantage Player
  | Game Player
  deriving (Show, Eq)


score :: Score -> Player -> Score
score Deuce winner = Advantage winner
score (Advantage player1) player2
  | player1 == player2 = Game player1
  | otherwise = Deuce
score (Forty FortyData {player=player1, otherPlayerPoint=point}) player2
  | player1 == player2 = Game player1
  | point == Thirty = Deuce
  | otherwise = Forty FortyData{player=player1, otherPlayerPoint=increment point}
  where
    new_point = increment point

increment Love = Fifteen
increment Fifteen = Thirty
