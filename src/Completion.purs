module Completion where

import Prelude
import Types (EnvRef, ShellST)
import Util (beginsWith)

import Data.Array (filter)

import Control.Monad.ST (ST, readSTRef)

import Node.ReadLine (Completer)

completer :: forall eff. EnvRef -> Completer ( st :: ST ShellST | eff)
completer e str = do
  env <- readSTRef e
  let matched = str
      completions = filter (_ `beginsWith` str) env.completions
  pure { completions: completions, matched }
