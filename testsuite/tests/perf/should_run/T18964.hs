import GHC.Exts
import Data.Int

main :: IO ()
main = do
  -- drop should fuse away and the program should consume O(1) space
  -- If fusion fails, this allocates about 640MB.
  print $ sum $ drop 10 [0..10000000::Int64]
  -- Here, drop can't fuse. This asserts that we don't regress in allocations in that case either
  -- If we don't do a good job here, we'll see more than 6.4MB of allocs.
  print $ lazy $ sum $ lazy $ drop 10 $ lazy [0..100000::Int64]

  -- and once more with dropWhile
  print $ sum $ dropWhile (< 10) [0..10000000::Int64]
  print $ lazy $ sum $ lazy $ dropWhile (< 10) $ lazy [0..100000::Int64]
