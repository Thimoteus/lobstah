module Main where

import Prelude
import Parser (parseCommand)
import Eval (eval)
import Types (IO, EnvRef, initialEnv)
import Completion (completer)

import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Path.Pathy (parseAbsDir)

import Control.Monad.Eff.Console (log)
import Control.Monad.ST (modifySTRef, readSTRef)
import Control.Apply ((*>))

import Node.ReadLine (Interface, setLineHandler, prompt, setPrompt, createInterface)
import Node.Process (lookupEnv, onBeforeExit)

main :: IO Unit
main = void do

  log "Welcome to lobstah!"

  onBeforeExit cleanUp

  init <- initialEnv
  interface <- createInterface $ completer init

  getInitData init
  setPrompt "> " 2 interface
  prompt interface

  let lineHandler :: EnvRef -> String -> IO Interface
      lineHandler e cmd = do

        env <- readSTRef e
        case parseCommand cmd env of

             Right cmd -> eval e interface cmd
             Left err -> log err *> prompt interface

  setLineHandler interface (lineHandler init)

getInitData :: EnvRef -> IO Unit
getInitData e = do

  whoami <- fromMaybe "user" <$> lookupEnv "USER"
  modifySTRef e (_ {whoami = whoami})

  home <- maybe "/" (_ <> "/") <$> lookupEnv "HOME"
  case parseAbsDir home of
       Just p -> void $ modifySTRef e (_ {home = p})
       _ -> pure unit

  cwd <- maybe "/" (_ <> "/") <$> lookupEnv "PWD"
  case parseAbsDir cwd of
       Just p -> void $ modifySTRef e (_ {cwd = p})
       _ -> pure unit

cleanUp :: IO Unit
cleanUp = log "bye!"
