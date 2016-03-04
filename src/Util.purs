module Util where

import Prelude

import Data.Path.Pathy (RelDir, Unsandboxed, Path, canonicalize, unsafePrintPath, unsandbox, currentDir)
import Data.Either (Either(..))
import Data.Maybe (Maybe(Just))
import Data.String (indexOf)

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (Error, EXCEPTION, catchException)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (message)

import Ansi.Codes (EscapeCode(Graphics), GraphicsParam(PMode, Reset), RenderingMode(Bold), escapeCodeToString)

showFsPath :: forall a b s. Path a b s -> String
showFsPath = unsafePrintPath <<< canonicalize

dotSlash :: RelDir Unsandboxed
dotSlash = unsandbox currentDir

catch :: forall a eff. Eff ( err :: EXCEPTION | eff ) a -> Eff eff (Either Error a)
catch e = catchException (pure <<< Left) (e >>= pure <<< Right)

bolden :: String -> String
bolden s = escapeCodeToString (Graphics [PMode Bold])
        <> s
        <> escapeCodeToString (Graphics [Reset])

release :: forall a b eff. Eff ( err :: EXCEPTION | eff ) a -> (Error -> Eff eff b) -> (a -> Eff eff b) -> Eff eff b
release unsafeCmd t k = do
  result <- catch unsafeCmd
  case result of
       Left err -> t err
       Right t -> k t

printErr :: forall eff. Error -> Eff ( console :: CONSOLE | eff ) Unit
printErr = log <<< message

unitize :: forall m a. Applicative m => a -> m Unit
unitize _ = pure unit

beginsWith :: String -> String -> Boolean
beginsWith super sub =
  case indexOf sub super of
      Just 0 -> true
      _ -> false
