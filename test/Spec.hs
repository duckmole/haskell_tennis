import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck
import Lib (Player(..), Score(..), Point(..), FortyData(..), score)

main :: IO ()
main = defaultMain tests

tests = [
        testGroup "Tennis score" [
                testProperty "Deuce score" deuce,
                testProperty "Advantage score when advantaged player wins" advantage_to_win,
                testProperty "Advantage score when advantaged player loses" advantage_to_deuce,
                testProperty "Given player: 40 when player wins then score is correct" forty_win,
                testProperty "Given player: 40 - other: 30 when other wins then score is correct" forty_to_deuce,
                testProperty "Given player: 40 - other: < 30 when other wins then score is correct" increment_point
                ]
      ]

instance Arbitrary Player where
  arbitrary = elements [Player1, Player2]

instance Arbitrary Point where
  arbitrary = elements [Love, Fifteen, Thirty]

newtype PointLessThirty = PointLessThirty Point
  deriving (Show, Eq)

instance Arbitrary PointLessThirty where
  arbitrary = elements [PointLessThirty Love, PointLessThirty Fifteen]

deuce winner =
  score Deuce winner == Advantage winner

advantage_to_win winner =
  score (Advantage winner) winner == Game winner

other Player1 = Player2
other Player2 = Player1

advantage_to_deuce winner =
  score (Advantage (other winner)) winner == Deuce

forty_win winner point =
  score (Forty FortyData{player=winner, otherPlayerPoint=point}) winner == Game winner

forty_to_deuce winner =
  score (Forty FortyData{player=other winner, otherPlayerPoint=Thirty}) winner == Deuce

increment_point :: Player -> PointLessThirty -> Bool
increment_point winner (PointLessThirty point) =
  score (Forty FortyData{player=other winner, otherPlayerPoint=point}) winner ==
  Forty FortyData{player=other winner, otherPlayerPoint=increment point}

increment Love = Fifteen
increment Fifteen = Thirty
