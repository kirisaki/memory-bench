module Main where

import Control.Concurrent
import Control.Monad
import Criterion.Main
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Storable
import Data.Word
import GHC.IO.Buffer
import System.Random

type Socket = ()

bufLen = 4096
bufNum = 100
threads = 1

recvBuf :: Socket -> Ptr Word8 -> Int -> IO Int
recvBuf _ ptr len = do
  forM_ (take len [0..]) $ \off -> do
    val <- randomIO :: IO Word8
    ptr `plusPtr` off `poke` val
  return len
  
withNewPinned :: IO ()
withNewPinned = replicateM_ bufNum (void $ allocaBytes bufLen (\ptr -> void $ recvBuf () ptr bufLen))

main :: IO ()
main = defaultMain
  [ bench "test" $ whnfIO $ withNewPinned
  ]
