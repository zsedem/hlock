module Graphics.X11.HLock(hlock) where
import ClassyPrelude
import Data.Bits((.|.))
import Control.Monad.Trans.State
import Control.Monad.Loops(whileM_)
import Control.Concurrent (threadDelay, forkIO)
import System.Exit(exitSuccess, exitFailure)
import System.Posix.User.Password
import qualified Graphics.X11.Xlib as X
import qualified Graphics.X11.Xrandr as X
import qualified Graphics.X11.Xlib.Extras as X

hlock :: MonadIO io => Bool -> io ()
hlock justTest = liftIO $ do
    dpy <- X.openDisplay ""
    locks <- forAllScreen dpy $ lockScreen dpy
    when justTest $ void $ forkIO $ do
        threadDelay (25 * 1000000)
        putStrLn "Exited after 15 sec because you set to just Testing"
        exitFailure
    waitForAuthenticate locks dpy
    forM_ locks $
        unlockScreen dpy
    X.closeDisplay dpy
    exitSuccess
  where
    unlockScreen _ _ = return ()

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

-- TODO better name here
waitForAuthenticate :: [LockT] -> X.Display -> IO ()
waitForAuthenticate locks dpy = runLoop $ do
    xEventPointer <- getEvent
    event <- liftIO $ X.getEvent xEventPointer
    liftIO $ print event
    case event of
        X.KeyEvent {} -> do
            (mayKsym, str) <- liftIO $ X.lookupString $ X.asKeyEvent xEventPointer
            let ksym = fromMaybe X.xK_F1 mayKsym
            if ksym == X.xK_KP_Enter || ksym == X.xK_Return
                then do
                    text <- getCharacters
                    resetCharacters
                    liftIO $ checkPassword text
                else do
                    unless ( X.ev_event_type event == X.keyRelease
                          || X.isFunctionKey ksym
                          || X.isKeypadKey ksym
                          || X.isMiscFunctionKey ksym
                          || X.isPFKey ksym
                          || X.isPrivateKeypadKey ksym) $
                        append str
                    return False
        _otherwise -> liftIO $ do
            forM_ locks $ \lock ->
                X.raiseWindow dpy $ window lock
            return False
  where
    runLoop loopBody = X.allocaXEvent $ \xevent -> evalStateT (whileM_ (not <$> loopBody) (return ())) (xevent, "")
    getEvent :: LoopBodyT X.XEventPtr
    getEvent = fst <$> get >>= \xe -> liftIO $ X.nextEvent dpy xe >> return xe
    getCharacters :: LoopBodyT String
    getCharacters = reverse.snd <$> get
    append :: String -> LoopBodyT ()
    append str = modify $ second (reverse str++)
    resetCharacters = modify $ second (const "")

type LoopBodyT a = StateT (X.XEventPtr, String) IO a

