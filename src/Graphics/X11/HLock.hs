module Graphics.X11.HLock(hlock) where
import ClassyPrelude (MonadIO, forM, liftIO, print, (=<<))
import Data.Bits((.|.))
import qualified Graphics.X11.Xlib as X
import qualified Graphics.X11.Xrandr as X

hlock :: MonadIO io => io ()
hlock = liftIO $ do
    dpy <- X.openDisplay ""
    let screenCount' = fromIntegral $ X.screenCount dpy
    _locks <- forM [0..screenCount' - 1] $ lockScreen dpy
    X.closeDisplay dpy

lockScreen :: X.Display -> X.ScreenNumber -> IO LockT
lockScreen dpy scr = X.allocaSetWindowAttributes $ \windowattributes -> do
    let black = X.blackPixel dpy scr
        visual = X.defaultVisual dpy scr
        depth = X.defaultDepth dpy scr
        displayWidth = fromIntegral $ X.displayWidth dpy scr
        displayHeight = fromIntegral $ X.displayHeight dpy scr
    rootWindow' <- X.rootWindow dpy scr
    X.set_background_pixel windowattributes black
    X.set_override_redirect windowattributes False
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
    -- add cursor hiding
    X.xrrSelectInput dpy newWindow X.rrScreenChangeNotifyMask
    X.mapWindow dpy newWindow
    X.raiseWindow dpy newWindow
    return $ Lock { screen = scr
                  , rootWindow = rootWindow'
                  , window = newWindow
                  , pmap = undefined
                  }
data LockT = Lock
    { screen :: X.ScreenNumber
    , rootWindow, window :: X.Window
    , pmap :: X.Pixmap
--    , unsigned long colors[NUMCOLS]
    }
