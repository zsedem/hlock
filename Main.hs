{-# LANGUAGE DeriveDataTypeable #-}
module Main(main) where
import Graphics.X11.HLock(hlock)
import System.Console.CmdArgs
data Arguments = Arguments
    {}
  deriving (Show, Data, Typeable)

arguments :: Arguments
arguments = Arguments
  {} &= summary "hlock v0.1.1.0 Simple Locker for Xorg display"

main :: IO ()
main = do
    _ <- cmdArgs arguments
    hlock
