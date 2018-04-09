{-Author: Wen Jiang-}
{-Email: wenjiang@uchicago.edu-}
{-Class: MPCS51400 Winter 2017-}

module FinalText ( footer ) where

import Printer

--ending msg
footer :: IO ()
footer = printText ["Do you want to play again or quit the game?","Please enter q or Q to quit. Please enter anything else to continue."]
