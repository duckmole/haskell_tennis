import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck
import Lib (Player(..), Point(..), ScoreMatch(..), score, sequenceGame)

main :: IO ()
main = defaultMain tests

tests = [
  testGroup "Sorting Group 1" [
      testProperty "Deuce score"
        (\winner -> score Deuce winner == Advantage winner),
      testProperty "Advantage score when player wins"
        (\winner -> score (Advantage winner) winner == Game winner),
      testProperty "Advantage score when other player wins"
        (\winner -> score (Advantage winner) (other winner) == Deuce),
      testProperty "40 player win"
        (\winner point -> score (Forty winner point) winner == Game winner),
      testProperty "40 player to deuce"
        (\winner -> score (Forty (other winner) Thirty) winner == Deuce),
      testProperty "40 player increment point"
        (\winner -> score (Forty (other winner) Love) winner == Forty (other winner) Fifteen),
      testProperty "Not game after 3 points" (balls 3 isNotGame),
      testProperty "Not deuce after 5 points" (balls 5 isNotDeuce),
      testProperty "Not advantage after 6 points" (balls 6 isNotAdvantage)
      ]
  ]


instance Arbitrary Player where
  arbitrary = elements [Player1, Player2]

instance Arbitrary Point where
  arbitrary = elements [Love, Fifteen, Thirty]

other Player1 = Player2
other Player2 = Player1

isNotGame:: ScoreMatch -> Bool
isNotGame (Game _) = False
isNotGame _ = True

isNotDeuce:: ScoreMatch -> Bool
isNotDeuce Deuce = False
isNotDeuce _ = True

isNotAdvantage:: ScoreMatch -> Bool
isNotAdvantage (Advantage _) = False
isNotAdvantage _ = True

balls:: Int -> (ScoreMatch -> Bool) -> [Player] -> Bool
balls number checker winners =
  checker (sequenceGame (take number winners))
