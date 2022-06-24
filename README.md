# WordleHs

This is a simple Wordle clone made with Haskell.

## Run Game

> Is necesary install [stack](https://docs.haskellstack.org/en/stable/README/) a package manager of Haskell.

```
stack run
```

## Explication

Do you remember the game called Wordle? The game that was trendy not long ago.

And sure, if you are a programmer, you have seen a lot of versions of this game with different languages and technologies. Now I'm going to show you, how to make this game in the language Haskell, loved by a few, hated by others and ignored by many.

Before you begin, it is necessary that install stack to install some packages.

The game read a list of words with animals.

```haskell
main :: IO ()
main = do
  let filename = "animals.txt"
  animalsText <- readFile filename
  ...
```

For this, I use `readFile` with the firm:

```haskell
readFile :: FilePath -> IO String
```

This means that when entering the file name we will receive an `IO String` and to “unwrap” the `IO` monad we will use the Haskell **do-notation**, that way we get the text with the file content.

Besides, to get a list with all words with the same format, we use the next code:

```haskell
import Control.Monad (join)
import Data.Text (toLower, pack)
import qualified Data.Text as T


main :: IO ()
main = do
  let filename = "animals.txt"
  animalsText <- readFile filename
  let animals =
        join
          . map T.words
          . T.lines
          . toLower
          . pack
          $ animalsText
  ...
```

You can imagine the process to build `animals` like a pipe that going from the bottom to the top. So `animalsText` first pass by `pack` that transform a `String` to a `Text`, `toLower` transform all text to lowercase, `lines` split the lines and `map T.words` to each line split in words and finally `join` all in a list.

In Haskell for use of random numbers (pseudo-randoms), we will use the package `random`. The next step is to choose a random word of the list. For this, we must make a seed:

```haskell
import System.Random (getStdGen, randomR)

main :: IO ()
main = do
  ...

  g <- getStdGen
  let selected_index = fst $ randomR (0 :: Int, length animals) g
  let selected_word = animals !! selected_index

  ...
```

The one I named `g`, later generate a random number, `selected_index` in the range 0 to the length of animals and getting the word in that index using `(!!)` function.

> In Haskell the operators also are functions.

We already have a random word chosen from a file, now we have to program the logic of the game, for this I created a `play` function.

```haskell
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
  ...
```

The `play` function:

```haskell
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
```

Let's go by parts, `play :: Text -> IO [Text]` receive the select word and return all games. In the definition of `play`, I created an auxiliary function called `go`, that receive the attempts and an empty list. This function iterate the games, i.e., ask the user a word, show the result and while as long as it does not run out of plays or win, the game continue in the recursion.

> The function `return` is not the same that the statement return of others languages mainly imperatives. In Haskell the function are composition of function, since what follows the equals is what defines the function.
> `return` wrap the value in a Monad, in this case `IO` Monad.

Other function important is `getWordle`, which is defined as:

```haskell
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
```

> The function (<>) is a operator to concat two Semigroups, in this case, two Text, for example `"foo" <> "bar" = "foobar"`.

This receives two `Text` types, `attemp` and `correct`, i.e., the attempt of user and the correct word. Haskell has the Let syntax, here we're going to define `result`.

The strategic that I thought was map the attempt and the correct word together, with some function that check if is success, misplace or fail. The function `zipWith` does that, apply `zip` to two lists and call a function with each pair values. In resume is a `map` and `zip` composed.

The `go` function, receive the two values and if are equals then marked as _Success_, if the character is in the correct word then is a _Misplace_ else is _Fail_.

If the attempt word is more shore than the correct then the result in screen was going to be more short too, to solve this `rest` contain the rest of result, but only if correct is more long than attempt. Lastly, `isCorrect` ask if `attempt` and `correct` are equals.

Also, I defined some others things like:

```haskell
data State = Fail | Success | Misplace

cshow :: State -> Char
cshow = \case
  Fail -> '⬜'
  Success -> '🟩'
  Misplace -> '🟨'

attemps :: Int
attemps = 6
```

I made a data type named `State` and defined `cshow` that return the character according to a `State`. And `attemps` that return the attempt amount of game.

Later of finish the game the program show the games, for this:

```haskell
main :: IO ()
main = do
  ...

  wordles <- play selected_word

  TIO.putStrLn . T.unlines . reverse $ wordles
```

> I use `putStrLn` from `Data.Text.IO` because the chars ['⬜', '🟩', '🟨'] are Unicode, of this way this characters are show in the console correctly.

## The Game in operation

```
$ stack run
Please enter your animal 1/6:
wolf
Rustle (ES) 1/6

⬜🟩⬜⬜⬜
Please enter your animal 2/6:
tortle
Rustle (ES) 2/6

⬜🟩🟩⬜⬜
Please enter your animal 3/6:
gorilla
Rustle (ES) 3/6

⬜🟩🟩⬜⬜
Please enter your animal 4/6:
orca
Rustle (ES) 4/6

🟨🟨⬜⬜⬜
Please enter your animal 5/6:
horse
Rustle (ES) 5/6

🟩🟩🟩🟩🟩
Congratulation!
Rustle (ES) 5/6

⬜🟩⬜⬜⬜
⬜🟩🟩⬜⬜
⬜🟩🟩⬜⬜
🟨🟨⬜⬜⬜
🟩🟩🟩🟩🟩
```
