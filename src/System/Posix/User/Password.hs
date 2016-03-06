module System.Posix.User.Password
  ( checkPassword
  ) where
import ClassyPrelude
import System.Posix.User
import System.Unix.Shadow
import System.Unix.Crypt

checkPassword :: String -> IO Bool
checkPassword password = do
    setEffectiveUserID 0
    pws <- sUserPassword <$> (getLoginName >>= getSUserEntryForName)
    (pws==) <$> crypt password pws

