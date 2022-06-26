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

attempts :: Int
attempts = 6

play :: Text -> IO [Text]
play selected_word = go attempts []
  where
    go :: Int -> [Text] -> IO [Text]
    go 0 xs = return xs
    go n xs = do
      let i = 1 + length xs

      putStrLn $ "Please enter your animal " ++ show i ++ "/" ++ show attempts ++ ": "

      attemptstr <- getLine
      let attemp = toLower . strip $ pack attemptstr
      let (wordle, correct) = getWordle attemp selected_word

      printf "Rustle (ES) %d/%d\n\n" i attempts

      TIO.putStrLn wordle

      if correct
        then do
          putStrLn "Congratulation!"
          printf "Rustle (ES) %d/%d\n\n" i attempts
          return (wordle : xs)
        else do
          go (n - 1) (wordle : xs)

getWordle :: Text -> Text -> (Text, Bool)
getWordle attempt correct =
  let result = T.zipWith go attempt correct
      rest =
        if T.length attempt < T.length correct
          then pack $ replicate (T.length correct - T.length attempt) $ cshow Fail
          else mempty
      isCorrect = attempt == correct
   in (result <> rest, isCorrect)
  where
    go :: Char -> Char -> Char
    go ca cc
      | ca == cc = cshow Success
      | ca `elem` unpack correct = cshow Misplace
      | otherwise = cshow Fail
