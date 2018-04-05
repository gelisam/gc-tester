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
  { _formStateLiveAmount            :: Double
  , _formStateGarbageAmount         :: Double
  , _formStateGarbageProducerCount  :: Int
  , _formStateGarbageProductionRate :: Seconds
  } deriving (Show)

makeLenses ''FormState

initialFormState :: FormState
initialFormState = FormState 0 0 0 0


data AppEvent
  = AppEventSetEvaluating Bool
  | AppEventUpdateStats   AppStats


data AppField
  = AppFieldLiveAmount
  | AppFieldGarbageAmount
  | AppFieldGarbageProducerCount
  | AppFieldGarbageProductionRate
  deriving (Show, Eq, Ord)

mkForm :: FormState -> Form FormState AppEvent AppField
mkForm = newForm
  [ (\field -> str "  amount of live data   " <+> field <+> str " constructors  ") @@= editShowableField formStateLiveAmount            AppFieldLiveAmount
  , (\field -> str "  amount of garbage     " <+> field <+> str " constructors  ") @@= editShowableField formStateGarbageAmount         AppFieldGarbageAmount
  , (\field -> str "  garbage producers     " <+> field <+> str      " threads  ") @@= editShowableField formStateGarbageProducerCount  AppFieldGarbageProducerCount
  , (\field -> str "  time between wakeups  " <+> field <+> str      " seconds  ") @@= editShowableField formStateGarbageProductionRate AppFieldGarbageProductionRate
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
  refLiveAmount            <- newIORef 0
  refGarbageAmount         <- newIORef 0
  refGarbageProducerCount  <- newIORef 0
  refGarbageProductionRate <- newIORef 0

  refLiveData <- newIORef (mkNat 0)
  refGarbageThreads <- newIORef []

  refGarbageDuration <- newIORef 0

  eventChan <- newBChan 10

  let produceGarbage :: IO ()
      produceGarbage = do
        garbageDuration <- measure $ do
          garbageAmount <- round <$> readIORef refGarbageAmount
          _ <- evaluate (fromNat (mkNat garbageAmount))
          pure ()

        writeIORef refGarbageDuration garbageDuration

        garbageProductionRate <- readIORef refGarbageProductionRate
        sleep (garbageProductionRate - realToFrac garbageDuration)
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

      updateLiveAmount :: Double -> IO ()
      updateLiveAmount doubleLiveAmount = do
        let liveAmount = round doubleLiveAmount
        writeIORef refLiveAmount doubleLiveAmount
        _ <- async $ do
          let liveData = mkNat liveAmount
          writeBChan eventChan . AppEventSetEvaluating $ True
          _ <- evaluate (fromNat liveData)
          writeBChan eventChan . AppEventSetEvaluating $ False
          writeIORef refLiveData liveData
        pure ()

      updateGarbageAmount :: Double -> IO ()
      updateGarbageAmount = writeIORef refGarbageAmount

      updateGarbageProducerCount :: Int -> IO ()
      updateGarbageProducerCount garbageProducerCount = do
        oldGarbageProducerCount <- readIORef refGarbageProducerCount
        oldGarbageThreads <- readIORef refGarbageThreads
        if garbageProducerCount < oldGarbageProducerCount
        then do
          mapM_ cancel (drop garbageProducerCount oldGarbageThreads)
          modifyIORef refGarbageThreads (take garbageProducerCount)
        else do
          newGarbageThreads <- replicateM (garbageProducerCount - oldGarbageProducerCount) (async produceGarbage)
          modifyIORef refGarbageThreads (++ newGarbageThreads)
        writeIORef refGarbageProducerCount garbageProducerCount

      updateGarbageProductionRate :: Seconds -> IO ()
      updateGarbageProductionRate = writeIORef refGarbageProductionRate

  rtsStats0 <- getRTSStats
  gcStatsThread <- async . measureGcStats $ rtsStats0

  let renderStats :: AppState -> (Widget AppField)
      renderStats s | view appStateEvaluating s = str "producing data..."
                    | otherwise
                    = (str "  each thread takes ~" <+> (str . show . view (appStateStats . appStatsGarbageDuration) $ s) <+> str " to produce its garbage")
                  <=> (str "  so we produce ~" <+> (str . show $ garbagePerSecond) <+> str " garbage constructors per second")
                  <=> str " "
                  <=> (str "  we run one GC every ~" <+> (str . show . view (appStateStats . appStatsGcRate) $ s) <+> str " seconds")
                  <=> (str "  the last one took " <+> (str . show . view (appStateStats . appStatsGcDuration) $ s) <+> str " seconds")
                  <=> (str "  and used " <+> (str . show . view (appStateStats . appStatsGcThreadCount) $ s) <+> str " threads")
        where
          garbagePerIteration :: Double
          garbagePerIteration = fromIntegral (view (appStateForm . to formState . formStateGarbageProducerCount) s)
                              * view (appStateForm . to formState . formStateGarbageAmount) s

          iterationDuration :: Seconds
          iterationDuration = view (appStateForm . to formState . formStateGarbageProductionRate) s
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
        liveAmount  <- liftIO . readIORef $ refLiveAmount
        garbageAmount  <- liftIO . readIORef $ refGarbageAmount
        garbageProducerCount  <- liftIO . readIORef $ refGarbageProducerCount
        garbageProductionRate <- liftIO . readIORef $ refGarbageProductionRate
        liftIO . updateIfChanged updateLiveAmount            liveAmount            . view (appStateForm . to formState . formStateLiveAmount)            $ s
        liftIO . updateIfChanged updateGarbageAmount         garbageAmount         . view (appStateForm . to formState . formStateGarbageAmount)         $ s
        liftIO . updateIfChanged updateGarbageProducerCount  garbageProducerCount  . view (appStateForm . to formState . formStateGarbageProducerCount)  $ s
        liftIO . updateIfChanged updateGarbageProductionRate garbageProductionRate . view (appStateForm . to formState . formStateGarbageProductionRate) $ s
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
  liveData <- readIORef refLiveData
  _ <- evaluate (fromNat liveData)
  putStrLn "done."
