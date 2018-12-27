{-# LANGUAGE Strict #-}
module Main where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Storable
import Data.Word
import System.Environment (getArgs)
import System.Random

type Socket = ()

bufSize = 128 * 1024
threads = 10000

recvBuf :: Socket -> Ptr Word8 -> Int -> IO Int
recvBuf _ ptr len = do
  randomRIO (0, 100) >>= threadDelay
  length <$> (forM (take len [0..]) $ \off -> 
                 ptr `plusPtr` off `poke` (0x00 :: Word8))

withBuf :: ((Ptr Word8 -> IO ()) -> IO ()) -> IO ()
withBuf io = do
  locks <- replicateM threads newEmptyMVar
  forM_ locks $ \l -> do
    randomRIO (0, 10000) >>= threadDelay
    forkIO . io $  \ptr ->
      void $ recvBuf () ptr bufSize
    putMVar l ()
  mapM_ takeMVar locks



withAlloca :: IO ()
withAlloca = withBuf $ allocaBytes bufSize


withMalloc :: IO ()
withMalloc = withBuf $ \io -> do
  ptr <- mallocBytes bufSize
  len <- io ptr
  free ptr

withMallocForeign :: IO ()
withMallocForeign = withBuf $ \io -> do
  ptr <- mallocBytes bufSize
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
