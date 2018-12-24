module Main where

import Control.Concurrent
import Control.Monad
import Criterion.Main
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Storable
import Data.Word
import GHC.IO.Buffer
import System.Random

type Socket = ()

bufLen = 4096
bufNum = 2
threads = 4

recvBuf :: Socket -> Ptr Word8 -> Int -> IO Int
recvBuf _ ptr len = do
  forM_ (take len [0..]) $ \off -> do
    val <- randomIO :: IO Word8
    ptr `plusPtr` off `poke` val
  return len

withBuf :: ((Ptr Word8 -> IO ()) -> IO ()) -> IO ()
withBuf f = replicateM_ bufNum (forkIO . void $ f (\ptr -> void $ recvBuf () ptr bufLen))

withAlloca :: IO ()
withAlloca = withBuf $ allocaBytes bufLen

withMalloc :: IO ()
withMalloc = do
  ptr <- mallocBytes bufLen
  ptr' <- newForeignPtr finalizerFree ptr
  withBuf $ withForeignPtr ptr'

main :: IO ()
main = defaultMain
  [ bench "alloca" $ whnfIO $ withAlloca
  , bench "malloc" $ whnfIO $ withMalloc
  ]
