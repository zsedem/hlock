module Graphics.X11.HLock(xLocker, hlock) where
import ClassyPrelude
import Data.Bits((.|.))
import Control.Monad.Trans.State
import Control.Concurrent(threadDelay)
import System.Exit(exitFailure)
import System.Posix.User.Password
import HLock hiding (hlock)
import qualified HLock(hlock)
import qualified Graphics.X11.Xlib as X
import qualified Graphics.X11.Xrandr as X
import qualified Graphics.X11.Xlib.Extras as X

xLocker :: LockerT LoopBodyT X.Display [LockT]
xLocker = Locker
    { grabResource = liftIO $ X.openDisplay ""
    , lockResource = \display ->
        liftIO $ forAllScreen display $ lockScreen display
    , unlockResource = \dpy _ -> liftIO $ X.closeDisplay dpy
    , authenticationTry = keyboardEventHandler
    }

hlock :: MonadIO io => io ()
hlock = runLoopBody $ HLock.hlock xLocker

runLoopBody :: MonadIO io => LoopBodyT a -> io a
runLoopBody loopBody = liftIO $ X.allocaXEvent $ \xevent ->
    evalStateT (unLoopBody loopBody) (xevent, "")

lockScreen :: X.Display -> X.ScreenNumber -> IO LockT
lockScreen dpy scr = do
    rootWindow' <- X.rootWindow dpy scr
    newWindow <- createBlackWindow dpy scr rootWindow'
    X.xrrSelectInput dpy newWindow X.rrScreenChangeNotifyMask
    grabPointer dpy rootWindow'
    grabKeyboard dpy rootWindow'
    return Lock { screen = scr
                , rootWindow = rootWindow'
                , window = newWindow
                }

data LockT = Lock
    { screen :: X.ScreenNumber
    , rootWindow, window :: X.Window
    }


grabPointer :: X.Display -> X.Window -> IO ()
grabPointer dpy rootWindow' = void $
    X.grabPointer dpy rootWindow' False (X.buttonPressMask .|. X.buttonReleaseMask .|. X.pointerMotionMask)
                  X.grabModeAsync X.grabModeAsync 0 0 X.currentTime


grabKeyboard :: X.Display -> X.Window -> IO ()
grabKeyboard dpy rw = do
    grabKeyboard' 100
    X.selectInput dpy rw X.substructureNotifyMask
  where
    grabKeyboard' :: Int -> IO ()
    grabKeyboard' tries | tries > 0 = do
        result<- X.grabKeyboard dpy rw True X.grabModeAsync X.grabModeAsync X.currentTime
        unless (result == X.grabSuccess) $ do
            threadDelay 100
            grabKeyboard' (tries - 1)
    grabKeyboard' _ = exitFailure


createBlackWindow :: X.Display -> X.ScreenNumber -> X.Window -> IO X.Window
createBlackWindow dpy scr rootWindow' = X.allocaSetWindowAttributes $ \windowattributes -> do
        X.set_background_pixel windowattributes $
            X.blackPixel dpy scr
        X.set_override_redirect windowattributes True
        newWindow <- X.createWindow
            dpy
            rootWindow'
            0
            0
            (fromIntegral $ X.displayWidth dpy scr)
            (fromIntegral $ X.displayHeight dpy scr)
            0
            (X.defaultDepth dpy scr)
            X.copyFromParent
            (X.defaultVisual dpy scr)
            (X.cWOverrideRedirect .|. X.cWBackPixel)
            windowattributes
        X.mapWindow dpy newWindow
        X.raiseWindow dpy newWindow
        return newWindow

forAllScreen :: X.Display -> (X.ScreenNumber -> IO b) -> IO [b]
forAllScreen dpy = forM [0..screenCount' - 1]
  where screenCount' = fromIntegral $ X.screenCount dpy

newtype LoopBodyT a = LoopBody
    { unLoopBody :: StateT (X.XEventPtr, String) IO a}
  deriving(Functor, Applicative, Monad, MonadIO)

keyboardEventHandler :: X.Display -> [LockT] -> LoopBodyT AuthorizedT
keyboardEventHandler dpy locks = do
    xEventPointer <- getEvent
    event <- liftIO $ X.getEvent xEventPointer
    case event of
        X.KeyEvent {} -> do
            (mayKsym, str) <- liftIO $ X.lookupString $ X.asKeyEvent xEventPointer
            let ksym = fromMaybe X.xK_F1 mayKsym
            if ksym == X.xK_KP_Enter || ksym == X.xK_Return
                then do
                    text <- getCharacters
                    resetCharacters
                    a <- liftIO $ checkPassword text
                    return $ case a of
                        True -> Authorized
                        False -> UnAuthorized
                else do
                    unless ( X.ev_event_type event == X.keyRelease
                          || X.isFunctionKey ksym
                          || X.isKeypadKey ksym
                          || X.isMiscFunctionKey ksym
                          || X.isPFKey ksym
                          || X.isPrivateKeypadKey ksym) $
                        append str
                    return Authorized
        _otherwise -> liftIO $ do
            forM_ locks $ \lock ->
                X.raiseWindow dpy $ window lock
            return UnAuthorized
  where
    getEvent :: LoopBodyT X.XEventPtr
    getEvent = LoopBody $ fst <$> get >>= \xe -> liftIO $ X.nextEvent dpy xe >> return xe
    getCharacters :: LoopBodyT String
    getCharacters = LoopBody ( reverse.snd <$> get)
    append :: String -> LoopBodyT ()
    append str = LoopBody (modify $ second (reverse str++))
    resetCharacters = LoopBody (modify $ second (const ""))


