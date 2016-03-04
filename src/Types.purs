module Types where

import Data.Path.Pathy (AbsDir, RelDir, AbsFile, RelFile, Unsandboxed, rootDir
                       ,unsandbox)
import Data.List (List(Nil))
import Data.Either (Either)
import Data.StrMap (StrMap, empty)

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.ST (ST, STRef, newSTRef)

import Node.Process (PROCESS)
import Node.FS (FS)
import Node.Buffer (BUFFER)

data ShellST

type Directory = AbsDir Unsandboxed

type AbsoFile = AbsFile Unsandboxed

type RelaFile = RelFile Unsandboxed

type File = Either AbsoFile RelaFile

data Dir = Absolute Directory
         | Relative (RelDir Unsandboxed)

type Thing = Either Dir File

type DirContents = { files :: List AbsoFile, dirs :: List AbsoFile }

emptyDirContents :: DirContents
emptyDirContents = { files: Nil, dirs: Nil }

type EnvRef = STRef ShellST Env

type Env = { cwd :: Directory
           , home :: Directory
           , whoami :: String
           , aliases :: StrMap String
           , history :: List Command
           , path :: List (AbsFile Unsandboxed)
           , completions :: Array String
           }

initialEnv :: IO EnvRef
initialEnv = newSTRef { cwd: unsandbox rootDir
                      , home: unsandbox rootDir
                      , whoami: ""
                      , aliases: empty :: StrMap String
                      , history: Nil
                      , path: Nil
                      , completions: []
                      }

type IO = Eff ( console :: CONSOLE
              , st :: ST ShellST
              , fs :: FS
              , buffer :: BUFFER
              , process :: PROCESS
              )

data Command = Cd Dir
             | Echo String
             | Touch File
             | Cat File
             | Ls (Array LsFlag) Dir
             | Mv Thing Thing
             | Cp Thing Thing
             | Rm Thing
             | MkDir Dir
             | Exec String
             | Pwd
             | Whoami
             | Exit

data LsFlag = LsAll
