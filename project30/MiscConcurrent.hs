import           Control.Concurrent (QSemN, newQSemN, signalQSemN, waitQSemN, threadDelay, forkIO)
import           Control.Concurrent.MVar (newMVar, putMVar, readMVar, takeMVar)
import           Control.Concurrent.QSem (QSem, newQSem, signalQSem, waitQSem)
import           Control.Concurrent.STM
import           Control.Monad (forM_)

{-
MVAR

Atomic Trans: No
Blocking: Yes
Hold value: Yes (or empty)
Best for: Simple locks
-}
mvarMain :: IO ()
mvarMain = do
    lock <- newMVar 0

    let task n = do
            v <- takeMVar lock  -- Lock acquired
            putStrLn $ "Task " ++ show n ++ " running"
            putStrLn $ "Current MVar is: " ++ show v
            threadDelay basicDelay
            putStrLn $ "Task " ++ show n ++ " done"
            putMVar lock (v + n)  -- Lock released
    
    forkIO $ task 1
    forkIO $ task 2
    forkIO $ task 3
    
    threadDelay (basicDelay * 4)
    readMVar lock >>= \v -> putStrLn $ "Final MVar is: " ++ show v
    where basicDelay = 1000000
    

{-
TVAR

Atomic Trans: Yes
Blocking: No
Hold value: Always has a value
Best for: Shared counters
-}
tvarmain :: IO ()
tvarmain = do
    tvar <- newTVarIO 0  -- Shared counter
    sem <- newQSem 1 -- https://stackoverflow.com/questions/32040536/haskell-forkio-threads-writing-on-top-of-each-other-with-putstrln    

    -- Create multiple threads updating the counter
    forM_ [1..5] $ \x -> forkIO $ do
        waitQSem sem        
        atomically $ modifyTVar' tvar (+1)
        putStrLn $ "Counter incremented from " ++ show x
        signalQSem sem        

    threadDelay 1  -- Wait for all updates
    waitQSem sem    
    finalValue <- readTVarIO tvar
    putStrLn $ "Final Counter Value: " ++ show finalValue
    signalQSem sem    

    -- Also see QSemN https://hackage.haskell.org/package/concurrency-1.1.1.0/docs/Control-Concurrent-Classy-QSemN.html
    -- sem <- newQSemN 100
    -- waitQSemN sem 100
    -- signalQSemN sem 99

{-
TMVAR

Atomic Trans: Yes
Blocking: Yes
Hold value: Yes (or empty)
Best for: Safe shared state
-}
tmvarMain :: IO ()
tmvarMain = do
    -- tmvar <- newTMVarIO "Initial Value"
    tmvar <- newEmptyTMVarIO

    forkIO $ do
        atomically $ putTMVar tmvar "Updated by Thread 1!"  -- Atomic update        

    threadDelay 1000000  -- Wait a bit

    value <- atomically $ takeTMVar tmvar  -- Atomic retrieval
    putStrLn $ "Main thread got: " ++ value

{-
TCHAN

Atomic Trans: Yes
Blocking: No
Hold value: Holds messages
Best for: Message queues
-}
tchanmain :: IO ()
tchanmain = do
    -- Create a new unbounded thread-safe channel
    chan <- newTChanIO

    -- Send some messages to the channel in different threads
    forkIO $ threadDelay 1000000 *> atomically (writeTChan chan "Hello from thread 1. (to Received 3)")
    forkIO $ threadDelay 1000 *> atomically (writeTChan chan "Hello from thread 2. (to Received 2)")        
    forkIO $ atomically $ writeTChan chan "Hello from thread 3. (to Received 1)"

    -- Read the messages from the channel and print them
    message1 <- atomically $ readTChan chan
    putStrLn $ "Received 1: " ++ message1

    message2 <- atomically $ readTChan chan
    putStrLn $ "Received 2: " ++ message2

    message3 <- atomically $ readTChan chan
    putStrLn $ "Received 3: " ++ message3

    putStrLn "Done receiving messages."

{-
QSEMN

https://hackage.haskell.org/package/concurrency-1.1.1.0/docs/Control-Concurrent-Classy-QSemN.html
-}
task :: QSemN -> Int -> IO ()
task sem n = do        
    threadDelay (n * 100000)
    putStrLn $ "Task " ++ show n ++ " is running..."
    signalQSemN sem 10

qsemnmain :: IO ()
qsemnmain = do
    sem <- newQSemN 0
    mapM_ (forkIO . task sem) [1..5]         
    waitQSemN sem 50
    putStrLn ("Done! Awaited all thread to release 50 credit!")

main :: IO ()
main = do
    mvarMain
    pmtdiv
    tvarmain
    pmtdiv
    tmvarMain
    pmtdiv
    tchanmain
    pmtdiv
    qsemnmain    
    where pmtdiv = putStrLn ("---------------------")

-- << OUTPUT
-- Task 1 running
-- Current MVar is: 0
-- Task 1 done
-- Task 2 running
-- Current MVar is: 1
-- Task 2 done
-- Task 3 running
-- Current MVar is: 3
-- Task 3 done
-- Final MVar is: 6
-- ---------------------
-- Counter incremented from 1
-- Counter incremented from 2
-- Counter incremented from 3
-- Counter incremented from 4
-- Counter incremented from 5
-- Final Counter Value: 5
-- ---------------------
-- Main thread got: Updated by Thread 1!
-- ---------------------
-- Received 1: Hello from thread 3. (to Received 1)
-- Received 2: Hello from thread 2. (to Received 2)
-- Received 3: Hello from thread 1. (to Received 3)
-- Done receiving messages.
-- ---------------------
-- Task 1 is running...
-- Task 2 is running...
-- Task 3 is running...
-- Task 4 is running...
-- Task 5 is running...
-- Done! Awaited all thread to release 50 credit!
-- >>