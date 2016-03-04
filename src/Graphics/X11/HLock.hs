module Graphics.X11.HLock(hlock) where
import ClassyPrelude
import Data.Bits((.|.))
import Data.Time.Clock.POSIX(getPOSIXTime)
import Control.Concurrent (threadDelay, forkIO)
import System.Exit(exitSuccess, exitFailure)
import qualified Graphics.X11.Xlib as X
import qualified Graphics.X11.Xrandr as X
import qualified Graphics.X11.Xlib.Extras as X

hlock :: MonadIO io => Bool -> io ()
hlock justTest = liftIO $ do
    dpy <- X.openDisplay ""
    locks <- forAllScreen dpy $ lockScreen dpy
    when justTest $ void $ forkIO $ do
        threadDelay (15 * 1000000)
        putStrLn "Exited after 15 sec because you set to just Testing"
        exitFailure
    readPassword
    X.closeDisplay dpy
    exitSuccess

lockScreen :: X.Display -> X.ScreenNumber -> IO LockT
lockScreen dpy scr = X.allocaSetWindowAttributes $ \windowattributes -> do
    blackColor <- initColor dpy "black"
    rootWindow' <- X.rootWindow dpy scr
    newWindow <- createBlackWindow dpy scr rootWindow'
    X.xrrSelectInput dpy newWindow X.rrScreenChangeNotifyMask
    X.grabPointer dpy rootWindow' False (X.buttonPressMask .|. X.buttonReleaseMask .|. X.pointerMotionMask)
                  X.grabModeAsync X.grabModeAsync 0 0 X.currentTime

    grabKeyboard dpy scr rootWindow'
    return Lock { screen = scr
                , rootWindow = rootWindow'
                , window = newWindow
                }

data LockT = Lock
    { screen :: X.ScreenNumber
    , rootWindow, window :: X.Window
    }

initColor dpy color = do
    let colormap = X.defaultColormap dpy (X.defaultScreen dpy)
    (apros,real) <- X.allocNamedColor dpy colormap color
    return apros

grabKeyboard dpy scr rw = do
    grabKeyboard' 100 dpy scr rw
    X.selectInput dpy rw X.substructureNotifyMask
  where
    grabKeyboard' try dpy scr rw | try > 0 = do
        result<- X.grabKeyboard dpy rw True X.grabModeAsync X.grabModeAsync X.currentTime
        unless (result == X.grabSuccess) $ do
            threadDelay 100
            grabKeyboard' (try - 1) dpy scr rw
    grabKeyboard' _ _ _ _ = exitFailure


createBlackWindow dpy scr rw = X.allocaSetWindowAttributes $ \windowattributes -> do
        X.set_background_pixel windowattributes $
            X.blackPixel dpy scr
        X.set_override_redirect windowattributes True
        window <- X.createWindow
            dpy
            rw
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
        X.mapWindow dpy window
        X.raiseWindow dpy window
        return window

forAllScreen dpy = forM [0..screenCount' - 1]
  where screenCount' = fromIntegral $ X.screenCount dpy

readPassword = undefined

