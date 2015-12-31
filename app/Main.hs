module Main where

import Lib ( expectedScore, varianceScore, stddevScore )

main :: IO ()
main = do
    putStrLn $ "expected: " ++ show expectedScore
    putStrLn $ "variance: " ++ show varianceScore
    putStrLn $ "stddev: " ++ show stddevScore

