{-Author: Wen Jiang-}
{-Email: wenjiang@uchicago.edu-}
{-Class: MPCS51400 Winter 2017-}

module InitText ( header ) where

import Printer

--beginning msg
header :: IO ()
header = printText ["Hi","Welcome to the game of BlackJack!"]
