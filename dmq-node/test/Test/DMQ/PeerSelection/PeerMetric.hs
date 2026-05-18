{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.DMQ.PeerSelection.PeerMetric (tests) where

import Control.Monad.Class.MonadTime.SI
import Data.Foldable qualified as Foldable
import Data.Function (on)
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Ord (Down (..))
import Data.OrdPSQ qualified as OrdPSQ

import DMQ.Diffusion.PeerSelection.PeerMetric as PeerMetric

import Test.Ouroboros.Network.Utils (Delay (..), renderRanges)
import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)


tests :: TestTree
tests = testGroup "Test.DMQ.PeerSelection.PeerMetric"
  [ testProperty "reportSigId"    prop_reportSigId
  , testProperty "erasePeer"      prop_erasePeer
  , testProperty "expired"        prop_expired
  , testProperty "announciness"   prop_announciness
  , testProperty "competingPeers" prop_competingPeers
  ]


type SigId    = Int
newtype PeerAddr = PeerAddr Int
  deriving (Eq, Ord, Show)

instance Arbitrary PeerAddr where
  -- We generate a small number of peers to increase the chance of multiple
  -- signatures from the same peer
  arbitrary = PeerAddr . getNonNegative  <$> resize 5 arbitrary
  shrink (PeerAddr addr) = PeerAddr <$> shrink addr

-- | Use the same time range as the sig generators so that the window
-- sometimes cuts into the generated sig history and sometimes spans all of it.
instance Arbitrary PeerMetricConfiguration where
  arbitrary = PeerMetricConfiguration . getDelay <$> resize 5200 arbitrary `suchThat` (> 0)
  shrink (PeerMetricConfiguration t) =
    [ PeerMetricConfiguration t'
    | Delay t' <- shrink (Delay t)
    , t' > 0
    ]

data SigType = SigValid     { sigId :: SigId, peerAddr :: PeerAddr, sigTime :: Time }
             | SigAnnounced { sigId :: SigId, peerAddr :: PeerAddr, sigTime :: Time }
             | SigInvalid   { sigId :: SigId, peerAddr :: PeerAddr, sigTime :: Time }
  deriving Show

instance Arbitrary SigType where
  arbitrary = frequency
    [ ( 6
      , SigValid <$> arbitrary
                 <*> arbitrary
                 <*> (Time . getDelay <$> resize 5200 arbitrary)
      )
    , ( 2
      , SigAnnounced <$> arbitrary
                     <*> arbitrary
                     <*> (Time . getDelay <$> resize 5200 arbitrary)
      )
    , ( 1
      , SigInvalid <$> arbitrary
                   <*> arbitrary
                   <*> (Time . getDelay <$> resize 5200 arbitrary)
      )
    ]
  shrink SigValid { sigId, peerAddr, sigTime = sigTime@(Time t) } =
    [ SigValid { sigId, peerAddr, sigTime = Time t' }
    | t' <- getDelay <$> shrink (Delay t)
    ]
    ++
    [ SigAnnounced { sigId, peerAddr, sigTime }
    , SigInvalid { sigId, peerAddr, sigTime }
    ]
  shrink SigAnnounced { sigId, peerAddr, sigTime = sigTime@(Time t) } =
    [ SigAnnounced { sigId, peerAddr, sigTime = Time t' }
    | t' <- getDelay <$> shrink (Delay t)
    ]
    ++
    [ SigInvalid { sigId, peerAddr, sigTime } ]
  shrink SigInvalid { sigId, peerAddr, sigTime = Time t } =
    [ SigInvalid { sigId, peerAddr, sigTime = Time t' }
    | t' <- getDelay <$> shrink (Delay t)
    ]

newtype Sigs = Sigs { getSigs :: NonEmpty SigType }
  deriving Show

fixupSigs :: NonEmpty SigType -> NonEmpty SigType
fixupSigs = NonEmpty.nubBy ((==) `on` sigId)

instance Arbitrary Sigs where
  arbitrary =
       Sigs
     . fixupSigs
     . NonEmpty.fromList
     . getNonEmpty
    <$> arbitrary
  shrink Sigs { getSigs = sigs } =
       Sigs
     . fixupSigs
     . NonEmpty.fromList
     . getNonEmpty
    <$> shrink (NonEmpty . NonEmpty.toList $ sigs)


reportSigType
  :: PeerMetricConfiguration
  -> (Map PeerAddr (LocalPeerMetricState SigId), PeerMetricState SigId PeerAddr)
  -> SigType
  -> (Map PeerAddr (LocalPeerMetricState SigId), PeerMetricState SigId PeerAddr)
reportSigType config (localStMap, st) = \case
    SigAnnounced { sigId, peerAddr, sigTime } ->
      let alterFn :: Maybe (LocalPeerMetricState SigId)
                  -> Maybe (LocalPeerMetricState SigId)
          alterFn Nothing  = Just $ reportSigIdImpl config sigId sigTime emptyLocalPeerMetricState
          alterFn (Just s) = Just $ reportSigIdImpl config sigId sigTime s
      in (Map.alter alterFn peerAddr localStMap, st)
    SigValid { sigId, peerAddr, sigTime } ->
      let localSt = reportSigIdImpl config sigId sigTime
                  $ Map.findWithDefault emptyLocalPeerMetricState peerAddr localStMap
          (localSt', st') = reportSigImpl config localSt (TraceLabelPeer peerAddr (sigId, TxAccepted)) st
      in (Map.insert peerAddr localSt' localStMap, st')
    SigInvalid { sigId, peerAddr, sigTime } ->
      let localSt = reportSigIdImpl config sigId sigTime
                  $ Map.findWithDefault emptyLocalPeerMetricState peerAddr localStMap
          (localSt', st') = reportSigImpl config localSt (TraceLabelPeer peerAddr (sigId, TxRejected)) st
      in (Map.insert peerAddr localSt' localStMap, st')


prepareState :: PeerMetricConfiguration
             -> NonEmpty SigType
             -> (Map PeerAddr (LocalPeerMetricState SigId) -> PeerMetricState SigId PeerAddr -> SigType -> Property)
             -> Property
prepareState config (sig :| sigs) k =
  let (localMap, st) = Foldable.foldl' (reportSigType config) (Map.empty, emptyPeerMetricState) (reverse sigs)
  in k localMap st sig


-- | Properties of `reportSigId`
--
-- * inserted entry has the right time
-- * old entries are pruned
-- * not pruned entries are preserved
--
prop_reportSigId :: PeerMetricConfiguration -> Sigs -> Property
prop_reportSigId config@PeerMetricConfiguration { timeWindowToKeep } (Sigs sigs) =
    prepareState config sigs $ \localStMap _st sig ->
    let peer :: PeerAddr
        peer  = peerAddr sig

        lst, lst' :: LocalPeerMetricState SigId
        lst   = Map.findWithDefault emptyLocalPeerMetricState peer localStMap
        lst'  = reportSigIdImpl config (sigId sig) (sigTime sig) lst

        lstM, lstM' :: Map SigId Time
        lstM  = Map.fromList
              . map (\(k,p,_) -> (k,p))
              . OrdPSQ.toList
              $ localState lst
        lstM' = Map.fromList $ map (\(k,p,_) -> (k,p)) $ OrdPSQ.toList $ localState lst'

        cutoff    = (-timeWindowToKeep) `addTime` sigTime sig
        numPruned = Map.size $ Map.filter (<= cutoff) lstM

    in  classify (cutoff > Time 0) "window < sigTime"
      . classify (numPruned > 0)   "entries pruned"
      . counterexample (show sig)
      . counterexample (show lstM)
      . counterexample (show lstM')
      $
        counterexample "wrong time inserted"
        (Map.lookup (sigId sig) lstM' === Just (sigTime sig))
      .&&.
        counterexample "old entries were pruned"
        ( case Map.elems lstM' of
            [] -> property True
            ks -> property $ minimum ks > cutoff
        )
      .&&.
        counterexample "not pruned entries were preserved"
        (Map.filter (> cutoff) lstM `Map.isSubmapOf` lstM')


-- | Check that `erasePeerImpl` deletes the peer and doesn't modify state of
-- other peers.
--
prop_erasePeer :: PeerMetricConfiguration -> Sigs -> Property
prop_erasePeer config (Sigs sigs) =
  prepareState config sigs $
  \_localStateMap st sig ->
    let peer = peerAddr sig
        a = announcinessImpl st
        st' = erasePeerImpl peer st
        a' = announcinessImpl st'
    in  label (if Map.member peer a then "peer present" else "peer absent")
      $ Map.delete peer a === a'

-- The first signature is valid, sigs are sorted by time, which hardens
-- the `prop_expired` test.
--
newtype SigsFirstValid = SigsFirstValid { getSigsFirstValid :: NonEmpty SigType }
  deriving Show

fromSigs :: Sigs -> SigsFirstValid
fromSigs (Sigs sigs) =
  SigsFirstValid
  -- TODO: with GHC-9.10 we can use `NonEmpty.sortOn`
  case List.sortOn (Down . sigTime) . NonEmpty.toList $ sigs of
    a : as ->
      SigValid
        { sigId = sigId a,
          peerAddr = peerAddr a,
          sigTime = sigTime a
        }
      :| as
    [] -> error "impossible happened"

instance Arbitrary SigsFirstValid where
  arbitrary = fromSigs <$> arbitrary
  shrink (SigsFirstValid as) = fromSigs `map` shrink (Sigs as)


-- | Verify that `reportSigId` and `reportSig` are pruning old entries.
--
prop_expired :: PeerMetricConfiguration -> SigsFirstValid -> Property
prop_expired config@PeerMetricConfiguration { timeWindowToKeep } (SigsFirstValid sigs) =
    prepareState config sigs $ \localStateMap st sig ->
    let time = (-timeWindowToKeep) `addTime` sigTime sig
        (localStateMap', st') = reportSigType config (localStateMap, st) sig
        lst  = Map.findWithDefault emptyLocalPeerMetricState (peerAddr sig) localStateMap
        lst' = localStateMap' Map.! peerAddr sig
        metricPruned = case OrdPSQ.minView (metricState st) of
          Just (_, oldest, _, _) -> oldest <= time
          Nothing                -> False
        localPruned  = case OrdPSQ.minView (localState lst) of
          Just (_, oldest, _, _) -> oldest <= time
          Nothing                -> False
    in  classify (time > Time 0)   "window < sigTime"
      . classify metricPruned      "metric entry pruned"
      . classify localPruned       "local entry pruned"
      . counterexample (show st) $
        counterexample (show st') $
        counterexample (show lst) $
        counterexample (show lst') $
      ( -- verify that old entries in `metricState` are pruned
        case OrdPSQ.minView (metricState st') of
          Nothing ->
            property True
          Just (_, oldest, _, _) ->
            case sig of
              SigValid {} ->
                counterexample (unwords [show oldest, "not later than", show time])
                  (oldest > time)
              SigInvalid {} ->
                property True
              SigAnnounced {} ->
                property True
      )
      .&&.
      ( -- verify that old entries in `LocalPeerMetricState` are pruned
        case OrdPSQ.minView (localState lst') of
          Nothing -> property True
          Just (_, oldest, _, _) ->
            case sig of
              SigValid {} ->
                counterexample (unwords [show oldest, "not later than", show time])
                  (oldest > time)
              SigInvalid {} ->
                property True
              SigAnnounced {} ->
                property True
      )


-- | Verify properties of `announcinessImpl`
--
-- * if signature was announced or invalid the metric map is not changed
-- * if signature is valid, the score increments relative to the pruned state
--   of old entries; untouched peers are preserved.
--
prop_announciness :: PeerMetricConfiguration -> Sigs -> Property
prop_announciness config@PeerMetricConfiguration { timeWindowToKeep } (Sigs sigs) =
    prepareState config sigs $ \localStMap st sig ->
    let a = announcinessImpl st
        (_localStMap', st') = reportSigType config (localStMap, st) sig
        a' = announcinessImpl st'
        cutoff       = (-timeWindowToKeep) `addTime` sigTime sig
        metricPruned = case OrdPSQ.minView (metricState st) of
          Just (_, oldest, _, _) -> oldest <= cutoff
          Nothing                -> False
    in  classify (cutoff > Time 0) "window < sigTime"
      . classify metricPruned      "metric entry pruned"
      . label ("max score: " ++ renderRanges 4 (if null a' then 0 else maximum a'))
      $ label ("number of peers: " ++ renderRanges 5 (Map.size a'))
      $ counterexample (show (st, a))
      $ counterexample (show (st', a'))
      $ case sig of
        SigValid { peerAddr, sigTime } ->
          -- Processing a `SigValid` at time `sigTime` prunes all entries with
          -- time `<= sigTime - timeWindowToKeep` (regardless of peer).  Compare
          -- against `st` pruned at the new sig's time so peers that lost
          -- entries to pruning are accounted for.
          let aPruned = announcinessImpl (pruneState sigTime st)
          in
               -- untouched entries are preserved
               Map.delete peerAddr aPruned === Map.delete peerAddr a'
          .&&.
               -- new entry is bumped & it is in the results
               Just (maybe 1 succ (Map.lookup peerAddr aPruned))
               ===
               Map.lookup peerAddr a'
        SigAnnounced {} ->
          a === a'
        SigInvalid { sigTime } ->
          let aPruned = announcinessImpl (pruneState sigTime st)
          in aPruned === a'
  where
    pruneState :: Time
               -> PeerMetricState SigId PeerAddr
               -> PeerMetricState SigId PeerAddr
    pruneState t PeerMetricState { metricState } =
      PeerMetricState {
        metricState =
            snd
          $ OrdPSQ.atMostView ((-timeWindowToKeep) `addTime` t) metricState
      }


-- | A pair of distinct peer addresses.
data TwoPeerAddrs = TwoPeerAddrs PeerAddr PeerAddr
  deriving Show

instance Arbitrary TwoPeerAddrs where
  arbitrary = do
    p <- arbitrary
    q <- arbitrary `suchThat` (/= p)
    return (TwoPeerAddrs p q)
  shrink (TwoPeerAddrs p q) =
    [ TwoPeerAddrs p' q'
    | (p', q') <- shrink (p, q)
    , p' /= q'
    ]


-- | Bundles a 'PeerMetricConfiguration' with two announcement times generated
-- at the same scale, so the time gap and the window are in comparable ranges.
data ConfigWithTimes = ConfigWithTimes
  { cwtConfig :: PeerMetricConfiguration
  , cwtTime1  :: Time
  , cwtTime2  :: Time
  } deriving Show

instance Arbitrary ConfigWithTimes where
  arbitrary = ConfigWithTimes
    <$> arbitrary
    <*> (Time . getDelay <$> resize 5200 arbitrary)
    <*> (Time . getDelay <$> resize 5200 arbitrary)
  shrink ConfigWithTimes { cwtConfig = config
                         , cwtTime1  = Time t1
                         , cwtTime2  = Time t2
                         } =
    [ ConfigWithTimes config' (Time t1') (Time t2')
    | (config', Delay t1', Delay t2') <- shrink (config, Delay t1, Delay t2)
    ]


-- | When two peers both get 'TxAccepted' for the same sigid (e.g. after a
-- mempool eviction and re-submission), the peer with the *earlier* announcement
-- time keeps the entry.  'peer1' is accepted first, 'peer2' second; the times
-- are independent so the test covers both displacement and no-displacement.
--
prop_competingPeers :: ConfigWithTimes -> SigId -> TwoPeerAddrs -> Property
prop_competingPeers ConfigWithTimes { cwtConfig = config
                                    , cwtTime1  = time1
                                    , cwtTime2  = time2
                                    }
                    sigId (TwoPeerAddrs peer1 peer2) =
  let lst1     = reportSigIdImpl config sigId time1 emptyLocalPeerMetricState
      lst2     = reportSigIdImpl config sigId time2 emptyLocalPeerMetricState
      -- peer1 is accepted first, claiming the entry
      (_, st1) = reportSigImpl config lst1 (TraceLabelPeer peer1 (sigId, TxAccepted)) emptyPeerMetricState
      -- peer2 is accepted second
      (_, st2) = reportSigImpl config lst2 (TraceLabelPeer peer2 (sigId, TxAccepted)) st1
      a        = announcinessImpl st2
      PeerMetricConfiguration { timeWindowToKeep } = config
      cutoff = (-timeWindowToKeep) `addTime` time2
  in if | time2 <= time1 -> label "displacement (peer2 announced earlier)" (a === Map.fromList [(peer2, 1)])
        | time1 > cutoff -> label "no displacement, peer1 keeps credit"    (a === Map.fromList [(peer1, 1)])
        | -- peer1 announced earlier, but its entry falls outside the window and is
          -- pruned when peer2's result arrives; peer2 is inserted fresh
          otherwise      -> label "no displacement, peer1 entry pruned"    (a === Map.fromList [(peer2, 1)])
