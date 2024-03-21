{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module       : XMonad.Hooks.EastGate
Description  : Adds support for exporting XMonad metrics via Prometheus.
License      : MIT

Portability  : unportable

Adds support for exporting XMonad metrics via <https://prometheus.io/ Prometheus>.
-}
module XMonad.Hooks.EastGate (
  -- * Usage
  -- $usage
  EGConfig (..),
  withMetrics,
) where

import Control.Concurrent (forkIO)
import Control.Monad (forM)
import Data.Map ((!))
import Data.Map qualified as M
import Data.Text qualified as T
import System.Metrics.Prometheus.Concurrent.Registry
import System.Metrics.Prometheus.Http.Scrape (serveMetrics)
import System.Metrics.Prometheus.Metric.Counter (Counter, inc)
import System.Metrics.Prometheus.MetricId
import XMonad
import XMonad.Prelude (keyToString)
import XMonad.Util.ExtensibleState qualified as XS

data Metrics = Metrics
  { stateChanges :: Counter
  , mouseFocuses :: Counter
  , keybindUses :: M.Map String Counter
  , windowsOpened :: M.Map String Counter
  , ensureWindowExists :: String -> M.Map String Counter -> IO (M.Map String Counter)
  }

instance ExtensionClass (Maybe Metrics) where
  initialValue = Nothing

-- | East Gate configuration.
newtype EGConfig = EGConfig
  { port :: Int
  -- ^ port to serve metrics from.
  }

instance Default EGConfig where
  def =
    EGConfig
      { port = 9532
      }

{- $usage
You can use this module with the following in your @xmonad.hs@:

> import XMonad.Hooks.EastGate
>
> main = xmonad $ ... . withMetrics def . ... $ def{...}

This will start a Prometheus exporter, that you can access on port 9532.
You can use the following example @prometheus.yml@ to view the metrics:

> global:
>   scrape_interval: 10s
> scrape_configs:
> - job_name: xmonad
>   static_configs:
>   - targets:
>     - localhost:9532

The hooks add support for tracking keybind usage, spawned windows and a couple
other arbitrary counters. All exported metrics are prefixed with "xmonad_".
-}

{- | Add Prometheus server and metrics to the given 'XConfig'.
See above for an example.
-}
withMetrics :: (LayoutClass l Window, Read (l Window)) => EGConfig -> XConfig l -> XConfig l
withMetrics c xmc =
  xmc
    { startupHook = startupHook xmc >> extraStartup c xmc
    , manageHook = extraManage >> manageHook xmc
    , keys = wrapKeys $ keys xmc $ xmc{layoutHook = Layout $ layoutHook xmc}
    , -- \^ keys is a function, for all the `modMask` and `terminal` self-referential stuff.
      -- this code is from XMonad.Main, so should be fine.
      logHook = logHook xmc >> extraLog
    }

wrapKeys :: M.Map (ButtonMask, KeySym) (X ()) -> XConfig Layout -> M.Map (ButtonMask, KeySym) (X ())
wrapKeys keys _ = M.mapWithKey ((>>) . inc' . keyToString) keys
 where
  inc' k = doInc k . fmap keybindUses =<< XS.get

  doInc _ Nothing = return ()
  doInc k (Just ms)
    | not (M.member k ms) = liftIO $ print $ "unknown key: " ++ k
    | otherwise = liftIO $ inc $ ms ! k

addWindow :: Registry -> String -> M.Map String Counter -> IO (M.Map String Counter)
addWindow reg s m =
  if M.member s m
    then return m
    else do
      cs <- registerCounter "xmonad_windows_opened" (fromList [("class", T.pack s)]) reg
      return $ M.insertWithKey (\_ _ k -> k) s cs m

extraStartup :: (LayoutClass l Window, Read (l Window)) => EGConfig -> XConfig l -> X ()
extraStartup cfg xmc = do
  reg <- liftIO new
  let ks = M.keys $ keys xmc $ xmc{layoutHook = Layout $ layoutHook xmc}
  keys <-
    liftIO
      $ forM
        ks
        ( \k -> do
            let k' = keyToString k
            c <- registerCounter "xmonad_keybind_uses" (fromList [("keys", T.pack k')]) reg
            return (k', c)
        )
  m <-
    liftIO
      $ Metrics
      <$> registerCounter "xmonad_state_changes" mempty reg
      <*> registerCounter "xmonad_mouse_focuses" mempty reg
      <*> return (M.fromList keys)
      <*> return M.empty
      <*> return (addWindow reg)
  XS.put $ Just m
  _ <- liftIO $ forkIO $ serveMetrics (port cfg) ["metrics"] (sample reg)
  return ()

extraManage :: Query ()
extraManage = do
  name <- liftX . getName =<< ask
  liftX
    $ XS.modifyM
    $ maybe
      (return Nothing)
      ( \ms -> do
          ws' <- liftIO $ ensureWindowExists ms name (windowsOpened ms)
          liftIO . inc $ ws' ! name
          return $ Just ms{windowsOpened = ws'}
      )

extraLog :: X ()
extraLog = do
  XS.modifyM
    $ maybe
      (return Nothing)
      ( \ms -> do
          liftIO $ inc $ stateChanges ms
          whenX (asks mouseFocused) $ liftIO $ inc $ mouseFocuses ms
          ms' <- maybe (return ms) (parseEvent ms) =<< asks currentEvent
          return $ Just ms'
      )
 where
  parseEvent ms ev = case eventName ev of
    "MapRequest" -> do return ms
    _ -> return ms

getName :: Window -> X String
getName win = do
  class' <- runQuery className win
  name' <- runQuery (stringProperty "WM_NAME") win
  return $ case (class', name') of
    ("", "") -> "undefined"
    ("", s) -> s
    (s, _) -> s
