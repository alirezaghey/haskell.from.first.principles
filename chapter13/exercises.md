# Solutions to problems of chapter 13

## Intermission: Check you understanding

Here is the import list from one of the modules in Chris's library called _blacktip_:
```hs
import qualified Control.Concurrent as CC
import qualified Control.Concurrent         as CC
import qualified Control.Concurrent.MVar    as MV
import qualified Data.ByteString.Char8      as B
import qualified Data.Locator               as DL
import qualified Data.Time.Clock.POSIX      as PSX
import qualified Filesystem                 as FS
import qualified Filesystem.Path.CurrentOS  as FPC
import qualified Network.Info               as NI
import qualified Safe
import Control.Exception          (mask, try)
import Control.Monad              (forever, when)
import Data.Bits
import Data.Bits.Bitwise          (fromListBE)
import Data.List.Split            (chunksOf)
import Database.Blacktip.Types
import System.IO.Unsafe           (unsafePerformIO)
```

For our purposes right now, it does not matter whether you are familiar with the modules referenced in the import list. Look at the declarations and answer the questions below:

1. What functions are being imported from `Control.Monad`?
<br>`forever` and `when`
2. Which imports are both unqualified and imported in their entirety?
<br> `Data.Bits` and `Database.Blacktip.Types`
3. From the name, what do you suppose importing `blacktip`s `Types` module brings in?
<br> type definitions for the project.
4. Now let's compare a small part of `blacktip`'s code to the above import list:

```hs
writeTimestamp  :: MV.MVar ServerState
                -> FPC.FilePath
                -> IO CC.ThreadId
writeTimestamp  s path = do
  CC.forkIO go
  where go = forever $ do
          ss <- MV.readMVar s
          mask $ \_ -> do
            FS.writeFile path
            (B.pack (show (ssTime ss)))
            -- sleep for 1 second
          CC.threadDelay 1000000
```
a. The type signature refers to three aliased imports. What modules are named in those aliases?
<br>**Answer:** `MV` is an alias for `Control.Concurrent.MVar`, `FPC` for `Filesystem.Path.CurrentOS`, and `CC` for `Control.Concurrent`.
<br>b. Which import does `FS.writeFile` refer to?
<br>**Answer:** `import qualified Filesystem as FS`
<br>c. Which import did `forever` come from?
<br>**Answer:** `import Control.Monad (forever, when)`


# Chapter exercises

## Hangman game logic

You may have noticed when you were playing with the hangman game, that there are some weird things about tis game logic:

 - alghtough it can play with words up to 9 characters long, you only get to guess 7 characters;
 - it ends the game after 7 guesses, whether they were correct or incorrect;
 - if your 7th guess supplies the last letter in the word, it may still tell you you lost;
 - it picks some very strange words that you didn't suspect were even in the dictionary.

These make it unlike hangman as you might have played it in the past. Ordinarily, only incorrect guesses count against you, so you can make as many correct guesses as you need to fill in the word. Modifying the game so that it either gives you more guesses before the game ends or only usese shorter words (or both) involves only a couple of uncomplicated steps.

A bit more cimplicated but worth attempting as an exercise is changing the game so that, as with normal hangman, only incorrrect guesses count towards the guess limit.


## Modifying code

1. Ciphers: Open your Ciphers module and modify it so that it works with user input.

[Solution file](exercise.files/cipher.hs)

2. Here is a very simple, short block of code. Notice it has a `forever` that will make it keep running, over and over again. Load it into your REPL and test it out. Then refer back to the chapter and modify it to exit successfully after a `False` result.

```hs
import Control.Monad

palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  case (line1 == reverse line1) of
    True -> putStrLn "It's a palindrome!"
    False -> putStrLn "Nope!"
```

```hs
import Control.Monad
import System.Exit (exitSuccess)
palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  case line1 == reverse line1 of
    True -> do
      putStrLn "It's a palindrome!"
      exitSuccess
    False -> putStrLn "Nope!"
```
[Solution file](exercise.files/palindrome.hs)


3. If you tried using `palindrome` on a sentence such as "Madam I'm Adam", you may have noticed that palindrome checker doesn't work on that. Modifying the above so that it works on sentences, too, involves several steps. You may need to refer back to previous examples in the chapter to get ideas for proper ordering and nesting. You may wish to import `Data.Char` to use the function `toLower`. Have fun.

```hs
import Control.Monad
import System.Exit (exitSuccess)
import Data.Char (toLower, ord)


palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  (if isPal line1 then
     (do putStrLn "It's a palindrome!"
         exitSuccess)
  else
     putStrLn "Nope!")

isPal :: [Char] -> Bool
isPal str = cleanStr == reverse cleanStr where
  cleanStr = filter (\x -> ord x >= ord 'a' && ord x <= ord 'z' ) $ map toLower str
```
[Solution file](exercise.files/palindrome2.hs)

4. For the following code:

```hs
type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

data PersonInvalid =
    NameEmpty
  | AgeTooLow
  | PersonInvalidUnknown String
  deriving (Eq, Show)

mkPerson  ::  Name
          ->  Age
          ->  Either PersonInvalid Person
mkPerson  name age
  | name /= "" && age > 0 = Right $ Person name age
  | name == ""            = Left NameEmpty
  | age <= 0              = Left AgeTooLow
  | otherwise             =
      Left $ PersonInvalidUnknown $
        "Name was: " ++ show name ++
        " Age was: " ++ show age
```

Your job is to write the following function _without_ modifying the code above.

```hs
gimmePerson :: IO ()
gimmePerson = undefined
```

Since `IO ()` is about the least informative type imaginable, we'll tell what it should do.

a. It should prompt the user for a name and age input.
<br> b. It should attempt to construct a `Person` value using the name and age the user entered. You'll need the `read` function for Age because it's an `Integer` rather than a `String`.
<br> c. If it constructed a successful person, it should print "Yay! Successfully got a person:" followed by the Person value.
<br> d. if it got an error value, report that an error occured and print the error.