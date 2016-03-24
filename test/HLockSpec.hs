module HLockSpec(spec) where
import ClassyPrelude
import Control.Monad.State
import qualified HLock
import Test.Hspec
import Test.QuickCheck
type TestMonad = State (Int,[Event])

testLocker :: HLock.LockerT TestMonad String TestLockT
testLocker = HLock.Locker
    { HLock.grabResource = pushEvent GrabResource >> return "resource"
    , HLock.lockResource = \_ -> do
        pushEvent LockResource
        return TestLock
    , HLock.authenticationTry = \_ _ -> tryAuth
    , HLock.unlockResource = \_ _ -> pushEvent UnlockResource
    }
  where
    tryAuth :: TestMonad HLock.AuthorizedT
    tryAuth = do
        (tries, events) <- get
        put (tries - 1, TryAuthenticate:events)
        return $ if tries > 0
            then HLock.UnAuthorized
            else HLock.Authorized
    pushEvent :: Event -> TestMonad ()
    pushEvent event = do
        (tries, events) <- get
        put (tries, event:events)


data TestLockT = TestLock deriving(Eq, Show)
data Event  = GrabResource | LockResource  | TryAuthenticate | UnlockResource deriving (Eq, Show)

spec :: SpecWith ()
spec = describe "hlock" $ do
    it "does call functions in order" $
      runTestHLock 0
        `shouldBe` [GrabResource,LockResource,TryAuthenticate,UnlockResource]
    it "always tries to authenticate again" $
      property $ \(Positive failures) ->
          countTries (runTestHLock failures) `shouldBe` (failures + 1)
  where
    runTestHLock failures = (reverse.snd) ( execState (HLock.hlock testLocker) (failures, []))
    countTries = length . filter (==TryAuthenticate)
