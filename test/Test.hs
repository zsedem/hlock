module Main(main) where
import AllSpec(spec)
import Test.Hspec.Runner
import Test.Hspec.Formatters

main :: IO ()
main = hspecWith defaultConfig {configFormatter = Just progress} spec
