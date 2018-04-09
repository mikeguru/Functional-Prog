{-Author: Wen Jiang-}
{-Email: wenjiang@uchicago.edu-}
{-Class: MPCS51400 Winter 2017-}

module Printer ( printText ) where

--print the inputted String
printText :: [String] -> IO ()
printText xs = mapM_ putStrLn xs
