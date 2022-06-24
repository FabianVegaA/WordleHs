module Main where

import Control.Monad (join)
import Data.Text
  ( pack,
    toLower,
  )
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Lib (play)
import System.Random (getStdGen, randomR)

main :: IO ()
main = do
  let filename = "animals.txt"
  animalsText <- readFile filename
  let animals =
        join
          . map T.words
          . T.lines
          . toLower
          $ pack animalsText

  g <- getStdGen
  let selected_index = fst $ randomR (0 :: Int, length animals) g
  let selected_word = animals !! selected_index

  wordles <- play selected_word

  TIO.putStrLn . T.unlines . reverse $ wordles
