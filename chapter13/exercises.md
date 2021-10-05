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




