{-# LANGUAGE ScopedTypeVariables #-}
--------------------------------------------------------------------------------
module Lib
    ( testReadWrite
    ) where
--------------------------------------------------------------------------------
import Database.LMDB.Simple
import Data.Monoid ((<>))
import Data.STRef
import Control.Monad.ST
import Control.Monad (forM_, void)
import Control.Monad.IO.Class
import Control.Exception (catch)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
--------------------------------------------------------------------------------
testReadWrite :: IO ()
testReadWrite = do
  env <- openEnvironment "test.db" defaultLimits :: IO (Environment ReadWrite)
  db <- readOnlyTransaction env $ getDatabase Nothing :: IO (Database String Int)


  -- forM_ [1..5] $ \worker -> forkIO $ do
  --   void $ previewTransaction env $ do
  --     liftIO $ threadDelay 100000
  --     put db "test" (Just 5)
  --     get db "test"

  --   putStrLn $ "Preview worker " <> show worker <> " finished"

  forM_ [1..5] $ \worker -> forkIO $ do
    void $ readOnlyTransaction env $ do
      liftIO $ threadDelay 100000
      get db "test"

    putStrLn $ "Read worker " <> show worker <> " finished"

  forM_ [1..5] $ \worker -> forkIO $ do
    void $ readWriteTransaction env $ do
      liftIO $ threadDelay 500000
      put db ("test" <> show worker) (Just 5)

    putStrLn $ "Write worker " <> show worker <> " finished"


  pure ()


previewTransaction
  :: (Mode tmode, SubMode emode tmode)
  => Environment  emode
  -> Transaction tmode a
  -> IO a
previewTransaction env tx = do
  mv <- newEmptyMVar
  transaction env (tx >>= liftIO . putMVar mv >> abort)
    `catch` (\(e :: AbortedTransaction)-> takeMVar mv)
