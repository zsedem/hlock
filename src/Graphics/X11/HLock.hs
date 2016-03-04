module Graphics.X11.HLock(hlock) where
import ClassyPrelude
import Data.Bits((.|.))
import Data.Time.Clock.POSIX(getPOSIXTime)
import Control.Concurrent (threadDelay)
import System.Exit(exitSuccess, exitFailure)
import qualified Graphics.X11.Xlib as X
import qualified Graphics.X11.Xrandr as X
import qualified Graphics.X11.Xlib.Extras as X

hlock :: MonadIO io => io ()
hlock = liftIO $ do
    dpy <- X.openDisplay ""
    let screenCount' = fromIntegral $ X.screenCount dpy
    locks <- forM [0..screenCount' - 1] $ lockScreen dpy
    threadDelay (10 * 1000000)
    exitSuccess
    X.closeDisplay dpy

lockScreen :: X.Display -> X.ScreenNumber -> IO LockT
lockScreen dpy scr = X.allocaSetWindowAttributes $ \windowattributes -> do
    let black = X.blackPixel dpy scr
        visual = X.defaultVisual dpy scr
        depth = X.defaultDepth dpy scr
        displayWidth = fromIntegral $ X.displayWidth dpy scr
        displayHeight = fromIntegral $ X.displayHeight dpy scr
    blackColor <- initColor dpy "black"
    rootWindow' <- X.rootWindow dpy scr
    X.set_background_pixel windowattributes black
    X.set_override_redirect windowattributes True
    newWindow <- X.createWindow
        dpy
        rootWindow'
        0
        0
        displayWidth
        displayHeight
        0
        depth
        X.copyFromParent
        visual
        (X.cWOverrideRedirect .|. X.cWBackPixel)
        windowattributes
    X.xrrSelectInput dpy newWindow X.rrScreenChangeNotifyMask
    X.mapWindow dpy newWindow
    X.raiseWindow dpy newWindow
    pmap' <- X.createPixmap dpy newWindow 5 10 (X.defaultDepthOfScreen (X.defaultScreenOfDisplay dpy))
    X.grabPointer dpy rootWindow' False (X.buttonPressMask .|. X.buttonReleaseMask .|. X.pointerMotionMask)
                  X.grabModeAsync X.grabModeAsync 0 0 X.currentTime

    grabKeyboard dpy scr rootWindow'
    X.selectInput dpy rootWindow' X.substructureNotifyMask
    return Lock { screen = scr
                , rootWindow = rootWindow'
                , window = newWindow
                }
data LockT = Lock
    { screen :: X.ScreenNumber
    , rootWindow, window :: X.Window
--    , pmap :: X.Pixmap
--    , unsigned long colors[NUMCOLS]
    }

initColor dpy color = do
    let colormap = X.defaultColormap dpy (X.defaultScreen dpy)
    (apros,real) <- X.allocNamedColor dpy colormap color
    return apros

grabKeyboard = grabKeyboard' 1000
  where
    grabKeyboard' try dpy scr rw | try > 0 = do
        result<- X.grabKeyboard dpy rw True X.grabModeAsync X.grabModeAsync X.currentTime
        unless (result == X.grabSuccess) $ do
            threadDelay 10
            grabKeyboard' (try - 1) dpy scr rw
    grabKeyboard' _ _ _ _ = exitFailure

