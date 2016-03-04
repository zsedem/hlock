module Main(main) where
import Graphics.X11.HLock(hlock)

main :: IO ()
main = hlock True
