module Lib
    ( expectedScore
    , varianceScore
    , stddevScore
    ) where

import Params
import Language.Haskell.TH
import Control.Monad ( replicateM )
import GHC.Base      ( build )
import Data.Ratio    ( (%) )

allScores :: [Int]
allScores = map score (permute [1..die])

permute :: [a] -> [[a]]
permute xs = $(do
    names <- replicateM gameLen (newName "x")
    return (CompE
           ((map (\x -> BindS (VarP x) (VarE (mkName "xs"))) names)
           ++ [NoBindS (ListE (map VarE names))])))

allScoresLen :: Integer
allScoresLen = fromIntegral (die ^ gameLen)

expectedScore :: Rational
expectedScore = ((sum . map fromIntegral $ allScores) % allScoresLen)

stddevScore :: Double
stddevScore = sqrt (fromRational varianceScore)

varianceScore :: Rational
varianceScore = (sum . map (\n -> ((fromIntegral n)-expectedScore) ^ (2::Int)) $ allScores) / fromInteger allScoresLen

score :: [Int] -> Int
score = sum . map (\(a,b) -> (a ^ (2::Int)) * b) . runLengthEncode

-- copied from:
-- https://wiki.haskell.org/99_questions/Solutions/10
runLengthEncode :: Eq a => [a] -> [(Int,a)]
runLengthEncode xs = build (\c n ->
  let
    f x r (Just a@(i,q)) | x == q    = r (Just (i+1,q))
                         | otherwise = a `c` r (Just (1, x))
    f x r Nothing = r (Just (1, x))

    final (Just a) = a `c` n
    final Nothing = n

  in
    foldr f final xs Nothing)

