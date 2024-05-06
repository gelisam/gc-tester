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
import Control.Lens hiding (zoom)
import Control.Monad
import Control.Monad.IO.Class
import Data.Function
import Data.Int
import Data.IORef
import Data.Time
import GHC.Stats
import GHC.Word
import Graphics.Vty (defaultConfig)
import Graphics.Vty.Attributes (defAttr)
import Graphics.Vty.Platform.Unix (mkVty)
import Graphics.Vty.Input.Events


type Seconds = Double

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


measure :: IO () -> IO NominalDiffTime
measure body = do
  t0 <- getCurrentTime
  _ <- body
  t1 <- getCurrentTime
  pure $ diffUTCTime t1 t0

sleep :: Seconds -> IO ()
sleep = threadDelay . round . (* 1000000)


every1 :: Seconds -> a -> (NominalDiffTime -> a -> IO a) -> IO ()
every1 rate x0 body = do
  t0 <- getCurrentTime
  flip fix (t0, x0) $ \loop (t, x) -> do
    t' <- getCurrentTime
    let dt = diffUTCTime t' t
    sleep (rate - realToFrac dt)

    t'' <- getCurrentTime
    let dt' = diffUTCTime t'' t
    x' <- body dt' x

    loop (t'', x')

every :: Seconds -> (NominalDiffTime -> IO ()) -> IO ()
every rate body = every1 rate () $ \dt () -> body dt


data AppStats = AppStats
  { _appStatsGarbageDuration             :: NominalDiffTime
  , _appStatsGcRate                      :: Maybe Seconds
  , _appStatsGcDuration                  :: Seconds
  , _appStatsGcThreadCount               :: Word32
  , _appStatsActualGarbageProductionRate :: Maybe NominalDiffTime
  , _appStatsActualStatsRefreshRate      :: Maybe NominalDiffTime
  } deriving (Show)

makeLenses ''AppStats

initialAppStats :: AppStats
initialAppStats = AppStats 0 Nothing 0 0 Nothing Nothing


data FormFields = FormFields
  { _formFieldsLiveAmount            :: Double
  , _formFieldsGarbageAmount         :: Double
  , _formFieldsGarbageProducerCount  :: Int
  , _formFieldsGarbageProductionRate :: Seconds
  , _formFieldsStatsRefreshRate      :: Seconds
  } deriving (Show)

makeLenses ''FormFields

initialFormFields :: FormFields
initialFormFields = FormFields 0 0 0 0 1


data AppEvent
  = AppEventSetEvaluating Bool
  | AppEventUpdateStats   AppStats


data AppField
  = AppFieldLiveAmount
  | AppFieldGarbageAmount
  | AppFieldGarbageProducerCount
  | AppFieldGarbageProductionRate
  | AppFieldStatsRefreshRate
  deriving (Show, Eq, Ord)

mkForm :: FormFields -> Form FormFields AppEvent AppField
mkForm = newForm
  [ (\field -> str "  amount of live data    " <+> field <+> str " constructors  ") @@= editShowableField formFieldsLiveAmount            AppFieldLiveAmount
  , (\field -> str "  amount of garbage      " <+> field <+> str " constructors  ") @@= editShowableField formFieldsGarbageAmount         AppFieldGarbageAmount
  , (\field -> str "  garbage producers      " <+> field <+> str      " threads  ") @@= editShowableField formFieldsGarbageProducerCount  AppFieldGarbageProducerCount
  , (\field -> str "  time between wakeups   " <+> field <+> str      " seconds  "
           <=> str " "                                                            ) @@= editShowableField formFieldsGarbageProductionRate AppFieldGarbageProductionRate
  , (\field -> str "  time between GC stats  " <+> field <+> str      " seconds  ") @@= editShowableField formFieldsStatsRefreshRate      AppFieldStatsRefreshRate
  ]

initialForm :: Form FormFields AppEvent AppField
initialForm = mkForm initialFormFields


data AppState = AppState
  { _appStateForm           :: Form FormFields AppEvent AppField
  , _appStateFormFields     :: FormFields
  , _appStateStats          :: AppStats
  , _appStateEvaluating     :: Bool
  , _appStateLiveDataThread :: Async ()
  , _appStateGarbageThreads :: [Async ()]
  , _appStateStatsThread    :: Async ()
  }

makeLenses ''AppState

mkAppState :: Async () -> [Async ()] -> Async () -> AppState
mkAppState = AppState initialForm initialFormFields initialAppStats False


main :: IO ()
main = do
  refLiveData <- newIORef (mkNat 0)
  refGarbageDuration <- newIORef 0
  eventChan <- newBChan 10
  refActualGarbageProductionRate <- newIORef Nothing

  let allocateLiveData :: Integer -> IO ()
      allocateLiveData liveAmount = do
        let liveData = mkNat liveAmount
        writeBChan eventChan . AppEventSetEvaluating $ True
        _ <- evaluate (fromNat liveData)
        writeBChan eventChan . AppEventSetEvaluating $ False
        writeIORef refLiveData liveData

      produceGarbage :: Integer -> Seconds -> IO ()
      produceGarbage garbageAmount garbageProductionRate = every garbageProductionRate $ \actualGarbageProductionRate -> do
        writeIORef refActualGarbageProductionRate . Just $ actualGarbageProductionRate

        garbageDuration <- measure $ do
          _ <- evaluate (fromNat (mkNat garbageAmount))
          pure ()
        writeIORef refGarbageDuration garbageDuration

      refreshStats :: Seconds -> IO ()
      refreshStats statsRefreshRate = do
        rtsStats0 <- getRTSStats
        t0 <- getCurrentTime
        every1 statsRefreshRate (rtsStats0, t0, Nothing) $ \actualStatsRefreshRate (rtsStats, t, gcRateMay) -> do
          rtsStats' <- getRTSStats
          t' <- getCurrentTime
          let dt = diffUTCTime t' t
              gcCount = major_gcs rtsStats' - major_gcs rtsStats
              gcRate' = realToFrac dt / fromIntegral gcCount

          garbageDuration             <- readIORef refGarbageDuration
          actualGarbageProductionRate <- readIORef refActualGarbageProductionRate
          let gcDuration    = nanoToSeconds . gcdetails_elapsed_ns . gc $ rtsStats'
              gcThreadCount = gcdetails_threads . gc $ rtsStats'
              appStats      = AppStats garbageDuration gcRateMay gcDuration gcThreadCount actualGarbageProductionRate (Just actualStatsRefreshRate)
          writeBChan eventChan . AppEventUpdateStats $ appStats

          if dt > 1 && gcCount > 5
          then pure (rtsStats', t', Just gcRate')
          else pure (rtsStats, t, gcRateMay)

      renderStats :: AppState -> (Widget AppField)
      renderStats s | view appStateEvaluating s = str "producing data..."
                    | otherwise
                    = (str "  each thread produces garbage every ~" <+> (str . maybe "?" show . view (appStateStats . appStatsActualGarbageProductionRate) $ s))
                  <=> (str "  and it takes ~" <+> (str . show . view (appStateStats . appStatsGarbageDuration) $ s) <+> str " to produce that garbage")
                  <=> (str "  so we produce ~" <+> (str . show $ garbagePerSecond) <+> str " garbage constructors per second")
                  <=> str " "
                  <=> (str "  we run one GC every ~" <+> (str . maybe "?" show . view (appStateStats . appStatsGcRate) $ s) <+> str " seconds")
                  <=> (str "  the last one took " <+> (str . show . view (appStateStats . appStatsGcDuration) $ s) <+> str " seconds")
                  <=> (str "  and used " <+> (str . show . view (appStateStats . appStatsGcThreadCount) $ s) <+> str " threads")
                  <=> str " "
                  <=> (str "  we update the GC stats above every ~" <+> (str . maybe "?" show . view (appStateStats . appStatsActualStatsRefreshRate) $ s))
        where
          garbagePerIteration :: Double
          garbagePerIteration = fromIntegral (view (appStateForm . to formState . formFieldsGarbageProducerCount) s)
                              * view (appStateForm . to formState . formFieldsGarbageAmount) s

          iterationDuration :: Seconds
          iterationDuration = view (appStateForm . to formState . formFieldsGarbageProductionRate) s
                        `max` realToFrac (view (appStateStats . appStatsGarbageDuration) s)

          garbagePerSecond :: Double
          garbagePerSecond = if iterationDuration == 0 then 0 else garbagePerIteration / iterationDuration

      renderAppState :: AppState -> [Widget AppField]
      renderAppState s = pure
                       $ borderWithLabel (str "GC tester")
                       $ center (renderForm . view appStateForm $ s)
                     <+> vBorder
                     <+> center (renderStats s)

      updateLiveAmount :: EventM AppField AppState ()
      updateLiveAmount = do
        oldLiveAmount <- use (appStateFormFields . formFieldsLiveAmount)
        newLiveAmount <- use (appStateForm . to formState . formFieldsLiveAmount)
        when (newLiveAmount /= oldLiveAmount) $ do
          oldLiveDataThread <- use appStateLiveDataThread
          newLiveDataThread <- liftIO $ do
            cancel oldLiveDataThread
            async $ do
              allocateLiveData (round newLiveAmount)
          appStateLiveDataThread .= newLiveDataThread

      updateGarbageProducers :: EventM AppField AppState ()
      updateGarbageProducers = do
        oldGarbageAmount         <- use (appStateFormFields . formFieldsGarbageAmount)
        oldGarbageProducerCount  <- use (appStateFormFields . formFieldsGarbageProducerCount)
        oldGarbageProductionRate <- use (appStateFormFields . formFieldsGarbageProductionRate)
        newGarbageAmount         <- use (appStateForm . to formState . formFieldsGarbageAmount)
        newGarbageProducerCount  <- use (appStateForm . to formState . formFieldsGarbageProducerCount)
        newGarbageProductionRate <- use (appStateForm . to formState . formFieldsGarbageProductionRate)
        when ( newGarbageAmount         /= oldGarbageAmount
            || newGarbageProducerCount  /= oldGarbageProducerCount
            || newGarbageProductionRate /= oldGarbageProductionRate
             ) $ do
          oldGarbageThreads <- use appStateGarbageThreads
          liftIO $ mapM_ cancel oldGarbageThreads
          newGarbageThreads <- liftIO $ replicateM newGarbageProducerCount . async $ do
            produceGarbage (round newGarbageAmount) newGarbageProductionRate
          appStateGarbageThreads .= newGarbageThreads

      updateStatsRefreshers :: EventM AppField AppState ()
      updateStatsRefreshers = do
        oldStatsRefreshRate <- use (appStateFormFields . formFieldsStatsRefreshRate)
        newStatsRefreshRate <- use (appStateForm . to formState . formFieldsStatsRefreshRate)
        when (newStatsRefreshRate /= oldStatsRefreshRate) $ do
          oldStatsThread <- use appStateStatsThread
          newStatsThread <- liftIO $ do
            cancel oldStatsThread
            async $ refreshStats newStatsRefreshRate
          appStateStatsThread .= newStatsThread

      updateFormFields :: EventM AppField AppState ()
      updateFormFields = do
        newFormFields <- use (appStateForm . to formState)
        appStateFormFields .= newFormFields

      updateAppState :: BrickEvent AppField AppEvent -> EventM AppField AppState ()
      updateAppState (VtyEvent (EvKey KEsc _)) = do
        halt
      updateAppState (VtyEvent (EvKey KEnter _)) = do
        updateLiveAmount
        updateGarbageProducers
        updateStatsRefreshers
        updateFormFields
      updateAppState (Brick.AppEvent (AppEventSetEvaluating evaluating)) = do
        appStateEvaluating .= evaluating
      updateAppState (Brick.AppEvent (AppEventUpdateStats appStats)) = do
        appStateStats .= appStats
      updateAppState e = do
        zoom appStateForm $ handleFormEvent e

      noStartEvent :: EventM AppField AppState ()
      noStartEvent = do
        pure ()

      noAttrMap :: AppState -> AttrMap
      noAttrMap _ = attrMap defAttr []

      app :: App AppState AppEvent AppField
      app = App renderAppState showFirstCursor updateAppState noStartEvent noAttrMap

  let initialLiveAmount            = view formFieldsLiveAmount            initialFormFields
      initialGarbageAmount         = view formFieldsGarbageAmount         initialFormFields
      initialGarbageProducerCount  = view formFieldsGarbageProducerCount  initialFormFields
      initialGarbageProductionRate = view formFieldsGarbageProductionRate initialFormFields
      initialStatsRefreshRate      = view formFieldsStatsRefreshRate      initialFormFields
  initialLiveDataThread <- async $ do
    allocateLiveData (round initialLiveAmount)
  initialGarbageThreads <- replicateM initialGarbageProducerCount . async $ do
    produceGarbage (round initialGarbageAmount) initialGarbageProductionRate
  initialStatsThread <- async $ do
    refreshStats initialStatsRefreshRate
  let initialAppState = mkAppState initialLiveDataThread initialGarbageThreads initialStatsThread
  vty <- mkVty defaultConfig
  appState <- customMain vty (mkVty defaultConfig) (Just eventChan) app initialAppState

  mapM_ cancel . view appStateGarbageThreads $ appState
  cancel . view appStateStatsThread $ appState

  putStrLn "to guarantee the GC knows the live data is live, we traverse all of it at the end of the program."
  putStrLn "feel free to abort the traversal using <Ctrl+C>."
  putStrLn ""
  putStrLn "traversing live data..."
  liveData <- readIORef refLiveData
  _ <- evaluate (fromNat liveData)
  putStrLn "done."
