{-# LANGUAGE BangPatterns, OverloadedStrings, RankNTypes, TemplateHaskell, ViewPatterns #-}
module Main where

import Brick
import Brick.BChan
import Brick.Forms
import Brick.Widgets.Border
import Brick.Widgets.Center
import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.Int
import Data.IORef
import Data.Time
import GHC.Stats
import GHC.Word
import Graphics.Vty (mkVty, defaultConfig)
import Graphics.Vty.Input.Events


type Seconds = Double

gcStatsUpdateRate :: Seconds
gcStatsUpdateRate = 5


data Nat = Z | S Nat

mkNat :: Integer -> Nat
mkNat 0 = Z
mkNat i = S (mkNat (i-1))

fromNat :: Nat -> Integer
fromNat = go 0
  where
    go !acc Z     = acc
    go !acc (S n) = go (acc+1) n


nanoToSeconds :: Int64 -> Seconds
nanoToSeconds nano = fromIntegral nano / 1e+9

updateIfChanged :: Eq a => (a -> IO ()) -> a -> a -> IO ()
updateIfChanged update x x' = when (x /= x') (update x')


measure :: IO () -> IO NominalDiffTime
measure body = do
  t0 <- getCurrentTime
  _ <- body
  t1 <- getCurrentTime
  pure $ diffUTCTime t1 t0

sleep :: Seconds -> IO ()
sleep = threadDelay . round . (* 1000000)


data AppStats = AppStats
  { _appStatsGarbageDuration :: NominalDiffTime
  , _appStatsGcRate          :: Seconds
  , _appStatsGcDuration      :: Seconds
  , _appStatsGcThreadCount   :: Word32
  } deriving (Show)

makeLenses ''AppStats

initialAppStats :: AppStats
initialAppStats = AppStats 0 0 0 0


data FormState = FormState
  { _formStateN  :: Double
  , _formStateK  :: Double
  , _formStateT  :: Int
  , _formStateDT :: Seconds
  } deriving (Show)

makeLenses ''FormState

initialFormState :: FormState
initialFormState = FormState 0 0 0 0


data AppEvent
  = AppEventSetEvaluating Bool
  | AppEventUpdateStats   AppStats


data AppField
  = AppFieldN
  | AppFieldK
  | AppFieldT
  | AppFieldDT
  deriving (Show, Eq, Ord)

mkForm :: FormState -> Form FormState AppEvent AppField
mkForm = newForm
  [ (\field -> str "  amount of live data   " <+> field <+> str " constructors  ") @@= editShowableField formStateN  AppFieldN
  , (\field -> str "  amount of garbage     " <+> field <+> str " constructors  ") @@= editShowableField formStateK  AppFieldK
  , (\field -> str "  garbage producers     " <+> field <+> str      " threads  ") @@= editShowableField formStateT  AppFieldT
  , (\field -> str "  time between wakeups  " <+> field <+> str      " seconds  ") @@= editShowableField formStateDT AppFieldDT
  ]

initialForm :: Form FormState AppEvent AppField
initialForm = mkForm initialFormState


data AppState = AppState
  { _appStateForm       :: Form FormState AppEvent AppField
  , _appStateStats      :: AppStats
  , _appStateEvaluating :: Bool
  }

makeLenses ''AppState

initialAppState :: AppState
initialAppState = AppState initialForm initialAppStats False


main :: IO ()
main = do
  refN  <- newIORef 0  -- amount of live data
  refK  <- newIORef 0  -- amount of garbage per thread per wakeup
  refT  <- newIORef 0  -- number of threads
  refDT <- newIORef 0  -- number of seconds betweek wakeups

  refNat <- newIORef (mkNat 0)
  refGarbageThreads <- newIORef []

  refGarbageDuration <- newIORef 0

  eventChan <- newBChan 10

  let produceGarbage :: IO ()
      produceGarbage = do
        garbageDuration <- measure $ do
          k <- round <$> readIORef refK
          _ <- evaluate (fromNat (mkNat k))
          pure ()

        writeIORef refGarbageDuration garbageDuration

        dt <- readIORef refDT
        sleep (dt - realToFrac garbageDuration)
        produceGarbage

      measureGcStats :: RTSStats -> IO ()
      measureGcStats rtsStats = do
        rtsStats' <- getRTSStats

        garbageDuration <- readIORef refGarbageDuration
        let gcCount       = major_gcs rtsStats' - major_gcs rtsStats
            gcRate        = gcStatsUpdateRate / fromIntegral gcCount
            gcDuration    = nanoToSeconds . gcdetails_elapsed_ns . gc $ rtsStats'
            gcThreadCount = gcdetails_threads . gc $ rtsStats'
            appStats      = AppStats garbageDuration gcRate gcDuration gcThreadCount
        writeBChan eventChan . AppEventUpdateStats $ appStats

        sleep gcStatsUpdateRate
        measureGcStats rtsStats'

      updateN :: Double -> IO ()
      updateN doubleN = do
        let n = round doubleN
        writeIORef refN doubleN
        _ <- async $ do
          let nat = mkNat n
          writeBChan eventChan . AppEventSetEvaluating $ True
          _ <- evaluate (fromNat nat)
          writeBChan eventChan . AppEventSetEvaluating $ False
          writeIORef refNat nat
        pure ()

      updateK :: Double -> IO ()
      updateK = writeIORef refK

      updateT :: Int -> IO ()
      updateT t = do
        oldT <- readIORef refT
        oldGarbageThreads <- readIORef refGarbageThreads
        if t < oldT
        then do
          mapM_ cancel (drop t oldGarbageThreads)
          modifyIORef refGarbageThreads (take t)
        else do
          newGarbageThreads <- replicateM (t-oldT) (async produceGarbage)
          modifyIORef refGarbageThreads (++ newGarbageThreads)
        writeIORef refT t

      updateDT :: Seconds -> IO ()
      updateDT = writeIORef refDT

  rtsStats0 <- getRTSStats
  gcStatsThread <- async . measureGcStats $ rtsStats0

  let renderStats :: AppState -> (Widget AppField)
      renderStats s | view appStateEvaluating s = str "evaluating..."
                    | otherwise
                    = (str "  each thread takes ~" <+> (str . show . view (appStateStats . appStatsGarbageDuration) $ s) <+> str " to produce its garbage")
                  <=> (str "  so we produce ~" <+> (str . show $ garbagePerSecond) <+> str " garbage constructors per second")
                  <=> str " "
                  <=> (str "  we run one GC every ~" <+> (str . show . view (appStateStats . appStatsGcRate) $ s) <+> str " seconds")
                  <=> (str "  the last one took " <+> (str . show . view (appStateStats . appStatsGcDuration) $ s) <+> str " seconds")
                  <=> (str "  and used " <+> (str . show . view (appStateStats . appStatsGcThreadCount) $ s) <+> str " threads")
        where
          garbagePerIteration :: Double
          garbagePerIteration = fromIntegral (view (appStateForm . to formState . formStateT) s)
                              * view (appStateForm . to formState . formStateK) s

          iterationDuration :: Seconds
          iterationDuration = view (appStateForm . to formState . formStateDT) s
                        `max` realToFrac (view (appStateStats . appStatsGarbageDuration) s)

          garbagePerSecond :: Double
          garbagePerSecond = if iterationDuration == 0 then 0 else garbagePerIteration / iterationDuration

      renderAppState :: AppState -> [Widget AppField]
      renderAppState s = pure
                       $ borderWithLabel (str "GC tester")
                       $ center (renderForm . view appStateForm $ s)
                     <+> vBorder
                     <+> center (renderStats s)

      updateAppState :: AppState -> BrickEvent AppField AppEvent -> EventM AppField (Next AppState)
      updateAppState s (VtyEvent (EvKey KEsc _)) = halt s
      updateAppState s (VtyEvent (EvKey KEnter _)) = do
        n  <- liftIO . readIORef $ refN
        k  <- liftIO . readIORef $ refK
        t  <- liftIO . readIORef $ refT
        dt <- liftIO . readIORef $ refDT
        liftIO . updateIfChanged updateN  n  . view (appStateForm . to formState . formStateN)  $ s
        liftIO . updateIfChanged updateK  k  . view (appStateForm . to formState . formStateK)  $ s
        liftIO . updateIfChanged updateT  t  . view (appStateForm . to formState . formStateT)  $ s
        liftIO . updateIfChanged updateDT dt . view (appStateForm . to formState . formStateDT) $ s
        continue s
      updateAppState s (Brick.AppEvent (AppEventSetEvaluating evaluating)) = do
        let s' = set appStateEvaluating evaluating s
        continue s'
      updateAppState s (Brick.AppEvent (AppEventUpdateStats appStats)) = do
        let s' = set appStateStats appStats s
        continue s'
      updateAppState s e = do
        s' <- traverseOf appStateForm (handleFormEvent e) s
        continue s'

      noAttrMap :: AppState -> AttrMap
      noAttrMap _ = forceAttrMap mempty

      app :: App AppState AppEvent AppField
      app = App renderAppState showFirstCursor updateAppState pure noAttrMap

  _ <- customMain (mkVty defaultConfig) (Just eventChan) app initialAppState

  cancel gcStatsThread

  putStrLn "to guarantee the GC knows the live data is live, we traverse all of it at the end of the program."
  putStrLn "feel free to abort the traversal using <Ctrl+C>."
  putStrLn ""
  putStrLn "traversing live data..."
  nat <- readIORef refNat
  _ <- evaluate (fromNat nat)
  putStrLn "done."
