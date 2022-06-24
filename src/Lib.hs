{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib (play) where

import Data.Text
  ( Text,
    pack,
    strip,
    toLower,
    unpack,
  )
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Printf (printf)

data State = Fail | Success | Misplace

cshow :: State -> Char
cshow = \case
  Fail -> '⬜'
  Success -> '🟩'
  Misplace -> '🟨'

attemps :: Int
attemps = 6

play :: Text -> IO [Text]
play selected_word = go attemps []
  where
    go :: Int -> [Text] -> IO [Text]
    go 0 xs = return xs
    go n xs = do
      let i = 1 + length xs

      putStrLn $ "Please enter your animal " ++ show i ++ "/" ++ show attemps ++ ": "

      attempStr <- getLine
      let attemp = toLower . strip $ pack attempStr
      let (wordle, correct) = getWordle attemp selected_word

      printf "Rustle (ES) %d/%d\n\n" i attemps

      TIO.putStrLn wordle

      if correct
        then do
          putStrLn "Congratulation!"
          printf "Rustle (ES) %d/%d\n\n" i attemps
          return (wordle : xs)
        else do
          go (n - 1) (wordle : xs)

getWordle :: Text -> Text -> (Text, Bool)
getWordle attemp correct =
  let result = T.zipWith go attemp correct
      rest =
        if T.length attemp < T.length correct
          then pack $ replicate (T.length correct - T.length attemp) $ cshow Fail
          else mempty
      isCorrect = attemp == correct
   in (result <> rest, isCorrect)
  where
    go :: Char -> Char -> Char
    go ca cc
      | ca == cc = cshow Success
      | ca `elem` unpack correct = cshow Misplace
      | otherwise = cshow Fail
