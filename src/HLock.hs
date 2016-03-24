module HLock(hlock, LockerT(..), AuthorizedT(..)) where
import ClassyPrelude
import Control.Monad.Loops(whileM_)

hlock :: Monad m => LockerT m resource lock -> m ()
hlock (Locker
        grabResource'
        lockResource'
        authenticationTry'
        unlockResource') = do
    resource <- grabResource'
    lock <- lockResource' resource
    whileM_ ((Authorized/=) <$> authenticationTry' resource lock) $ return ()
    unlockResource' resource lock

data LockerT monad resource lock = Locker
    { grabResource :: monad resource
    , lockResource :: resource -> monad lock
    , authenticationTry :: resource -> lock -> monad AuthorizedT
    , unlockResource :: resource -> lock -> monad ()
    }

data AuthorizedT = Authorized | UnAuthorized deriving(Eq, Show)

