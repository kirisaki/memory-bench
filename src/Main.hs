{-# LANGUAGE Strict #-}
module Main where

import Control.Concurrent
import Control.DeepSeq
import Control.Exception (evaluate)
import Control.Monad
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Storable
import Data.Word
import GHC.IO.Buffer
import System.Environment (getArgs)
import System.Random

type Socket = ()

bufLen = 4096
bufNum = 2000
rounds = 10
threads = 100000
latency = 1000
interval = 0

recvBuf :: Socket -> Ptr Word8 -> Int -> IO Int
recvBuf _ ptr len = do
  forM_ (take len [0..]) $ \off -> do
    val <- randomIO :: IO Word8
    threadDelay latency
    ptr `plusPtr` off `poke` val
  return len

withBuf :: ((Ptr Word8 -> IO ()) -> IO ()) -> IO ()
withBuf f = replicateM_ rounds
  (threadDelay interval >> replicateM_ threads
    (forkIO . replicateM_ bufNum $ f
     (\ptr -> 
         void $ recvBuf () ptr bufLen
     )
    )
  )


withAlloca :: IO ()
withAlloca = withBuf $ allocaBytes bufLen

withMalloc :: IO ()
withMalloc = withBuf $ \io -> do
  ptr <- mallocBytes bufLen
  io ptr
  free ptr

withMallocForeign :: IO ()
withMallocForeign = withBuf $ \io -> do
  ptr <- mallocBytes bufLen
  ptr' <- newForeignPtr finalizerFree ptr
  withForeignPtr ptr' io

main :: IO ()
main = do
  args <- getArgs
  case args !! 0 of
    "1" ->
      putStrLn "withAlloca" >> withAlloca
    "2" ->
      putStrLn "withMalloc" >> withMalloc
    "3" ->
      putStrLn "withMallocForeign" >> withMallocForeign
