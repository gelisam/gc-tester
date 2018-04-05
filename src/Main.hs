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
import Data.Function
import Data.Int
import Data.IORef
import Data.Time
import GHC.Stats
import GHC.Word
import Graphics.Vty (mkVty, defaultConfig)
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
  { _appStatsGarbageDuration :: NominalDiffTime
  , _appStatsGcRate          :: Maybe Seconds
  , _appStatsGcDuration      :: Seconds
  , _appStatsGcThreadCount   :: Word32
  } deriving (Show)

makeLenses ''AppStats

initialAppStats :: AppStats
initialAppStats = AppStats 0 Nothing 0 0


data FormState = FormState
  { _formStateLiveAmount            :: Double
  , _formStateGarbageAmount         :: Double
  , _formStateGarbageProducerCount  :: Int
  , _formStateGarbageProductionRate :: Seconds
  , _formStateStatsRefreshRate      :: Seconds
  } deriving (Show)

makeLenses ''FormState

initialFormState :: FormState
initialFormState = FormState 0 0 0 0 1


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

mkForm :: FormState -> Form FormState AppEvent AppField
mkForm = newForm
  [ (\field -> str "  amount of live data    " <+> field <+> str " constructors  ") @@= editShowableField formStateLiveAmount            AppFieldLiveAmount
  , (\field -> str "  amount of garbage      " <+> field <+> str " constructors  ") @@= editShowableField formStateGarbageAmount         AppFieldGarbageAmount
  , (\field -> str "  garbage producers      " <+> field <+> str      " threads  ") @@= editShowableField formStateGarbageProducerCount  AppFieldGarbageProducerCount
  , (\field -> str "  time between wakeups   " <+> field <+> str      " seconds  "
           <=> str " "                                                            ) @@= editShowableField formStateGarbageProductionRate AppFieldGarbageProductionRate
  , (\field -> str "  time between GC stats  " <+> field <+> str      " seconds  ") @@= editShowableField formStateStatsRefreshRate      AppFieldStatsRefreshRate
  ]

initialForm :: Form FormState AppEvent AppField
initialForm = mkForm initialFormState


data AppState = AppState
  { _appStateForm           :: Form FormState AppEvent AppField
  , _appStateFormState      :: FormState
  , _appStateStats          :: AppStats
  , _appStateEvaluating     :: Bool
  , _appStateLiveDataThread :: Async ()
  , _appStateGarbageThreads :: [Async ()]
  , _appStateStatsThread    :: Async ()
  }

makeLenses ''AppState

mkAppState :: Async () -> [Async ()] -> Async () -> AppState
mkAppState = AppState initialForm initialFormState initialAppStats False


main :: IO ()
main = do
  refLiveData <- newIORef (mkNat 0)
  refGarbageDuration <- newIORef 0
  eventChan <- newBChan 10

  let allocateLiveData :: Integer -> IO ()
      allocateLiveData liveAmount = do
        let liveData = mkNat liveAmount
        writeBChan eventChan . AppEventSetEvaluating $ True
        _ <- evaluate (fromNat liveData)
        writeBChan eventChan . AppEventSetEvaluating $ False
        writeIORef refLiveData liveData

      produceGarbage :: Integer -> Seconds -> IO ()
      produceGarbage garbageAmount garbageProductionRate = every garbageProductionRate $ \_ -> do
        garbageDuration <- measure $ do
          _ <- evaluate (fromNat (mkNat garbageAmount))
          pure ()
        writeIORef refGarbageDuration garbageDuration

      refreshStats :: Seconds -> IO ()
      refreshStats statsRefreshRate = do
        rtsStats0 <- getRTSStats
        t0 <- getCurrentTime
        every1 statsRefreshRate (rtsStats0, t0, Nothing) $ \_ (rtsStats, t, gcRateMay) -> do
          rtsStats' <- getRTSStats
          t' <- getCurrentTime
          let dt = diffUTCTime t' t
              gcCount = major_gcs rtsStats' - major_gcs rtsStats
              gcRate' = realToFrac dt / fromIntegral gcCount

          garbageDuration <- readIORef refGarbageDuration
          let gcDuration    = nanoToSeconds . gcdetails_elapsed_ns . gc $ rtsStats'
              gcThreadCount = gcdetails_threads . gc $ rtsStats'
              appStats      = AppStats garbageDuration gcRateMay gcDuration gcThreadCount
          writeBChan eventChan . AppEventUpdateStats $ appStats

          if dt > 1 && gcCount > 5
          then pure (rtsStats', t', Just gcRate')
          else pure (rtsStats, t, gcRateMay)

      renderStats :: AppState -> (Widget AppField)
      renderStats s | view appStateEvaluating s = str "producing data..."
                    | otherwise
                    = (str "  each thread takes ~" <+> (str . show . view (appStateStats . appStatsGarbageDuration) $ s) <+> str " to produce its garbage")
                  <=> (str "  so we produce ~" <+> (str . show $ garbagePerSecond) <+> str " garbage constructors per second")
                  <=> str " "
                  <=> (str "  we run one GC every ~" <+> (str . maybe "?" show . view (appStateStats . appStatsGcRate) $ s) <+> str " seconds")
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

      updateLiveAmount :: AppState -> EventM AppField AppState
      updateLiveAmount s = liftIO $ do
        let oldLiveAmount = view (appStateFormState . formStateLiveAmount) s
            newLiveAmount = view (appStateForm . to formState . formStateLiveAmount)  s
        if newLiveAmount /= oldLiveAmount
        then flip (traverseOf appStateLiveDataThread) s $ \oldLiveDataThread -> do
          cancel oldLiveDataThread
          async $ do
            allocateLiveData (round newLiveAmount)
        else pure s

      updateGarbageProducers :: AppState -> EventM AppField AppState
      updateGarbageProducers s = liftIO $ do
        let oldGarbageAmount         = view (appStateFormState . formStateGarbageAmount)         s
            oldGarbageProducerCount  = view (appStateFormState . formStateGarbageProducerCount)  s
            oldGarbageProductionRate = view (appStateFormState . formStateGarbageProductionRate) s
            newGarbageAmount         = view (appStateForm . to formState . formStateGarbageAmount)         s
            newGarbageProducerCount  = view (appStateForm . to formState . formStateGarbageProducerCount)  s
            newGarbageProductionRate = view (appStateForm . to formState . formStateGarbageProductionRate) s
        if   newGarbageAmount         /= oldGarbageAmount
          || newGarbageProducerCount  /= oldGarbageProducerCount
          || newGarbageProductionRate /= oldGarbageProductionRate
        then flip (traverseOf appStateGarbageThreads) s $ \oldGarbageThreads -> do
          mapM_ cancel oldGarbageThreads
          replicateM newGarbageProducerCount . async $ do
            produceGarbage (round newGarbageAmount) newGarbageProductionRate
        else pure s

      updateStatsRefreshers :: AppState -> EventM AppField AppState
      updateStatsRefreshers s = liftIO $ do
        let oldStatsRefreshRate = view (appStateFormState . formStateStatsRefreshRate) s
            newStatsRefreshRate = view (appStateForm . to formState . formStateStatsRefreshRate)  s
        if newStatsRefreshRate /= oldStatsRefreshRate
        then flip (traverseOf appStateStatsThread) s $ \oldStatsThread -> do
          cancel oldStatsThread
          async $ do
            refreshStats newStatsRefreshRate
        else pure s

      updateFormState :: AppState -> EventM AppField AppState
      updateFormState s = do
        let newFormState = view (appStateForm . to formState) s
        pure . set (appStateFormState) newFormState $ s

      updateAppState :: AppState -> BrickEvent AppField AppEvent -> EventM AppField (Next AppState)
      updateAppState s (VtyEvent (EvKey KEsc _)) = halt s
      updateAppState s (VtyEvent (EvKey KEnter _)) = pure s
                                                 >>= updateLiveAmount
                                                 >>= updateGarbageProducers
                                                 >>= updateStatsRefreshers
                                                 >>= updateFormState
                                                 >>= continue
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

  let initialLiveAmount            = view formStateLiveAmount            initialFormState
      initialGarbageAmount         = view formStateGarbageAmount         initialFormState
      initialGarbageProducerCount  = view formStateGarbageProducerCount  initialFormState
      initialGarbageProductionRate = view formStateGarbageProductionRate initialFormState
      initialStatsRefreshRate      = view formStateStatsRefreshRate      initialFormState
  initialLiveDataThread <- async $ do
    allocateLiveData (round initialLiveAmount)
  initialGarbageThreads <- replicateM initialGarbageProducerCount . async $ do
    produceGarbage (round initialGarbageAmount) initialGarbageProductionRate
  initialStatsThread <- async $ do
    refreshStats initialStatsRefreshRate
  let initialAppState = mkAppState initialLiveDataThread initialGarbageThreads initialStatsThread
  appState <- customMain (mkVty defaultConfig) (Just eventChan) app initialAppState

  mapM_ cancel . view appStateGarbageThreads $ appState
  cancel . view appStateStatsThread $ appState

  putStrLn "to guarantee the GC knows the live data is live, we traverse all of it at the end of the program."
  putStrLn "feel free to abort the traversal using <Ctrl+C>."
  putStrLn ""
  putStrLn "traversing live data..."
  liveData <- readIORef refLiveData
  _ <- evaluate (fromNat liveData)
  putStrLn "done."
