{-# LANGUAGE PackageImports, CPP #-}
module Prelude
  ( module P
  ) where
import "basic-prelude" CorePrelude as P
import "base" Prelude as P((++), reverse)

