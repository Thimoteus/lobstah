module Eval where

import Prelude
import Types (IO, EnvRef, Env, Directory, Command(..), Dir(..), LsFlag(..), DirContents)
import Util (showFsPath, bolden, release, printErr, unitize, catch)

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.List (List(Nil), (:), mapMaybe, null, fromFoldable)
import Data.Path.Pathy ((</>), unsafePrintPath, canonicalize, fileName, runFileName)
import Data.Array (zip)
import Data.String (charAt)
import Data.Foldable (foldl, intercalate)
import Data.Traversable (traverse)
import Data.Tuple (fst, snd)

import Control.Monad (when)
import Control.Monad.Eff.Console (log)
import Control.Monad.ST (readSTRef, modifySTRef)
import Control.Apply ((*>), (<*))

import Node.FS.Sync (exists, writeFile, readdir, stat, readTextFile, mkdir, rmdir, unlink)
import Node.FS.Stats (isDirectory)
import Node.Encoding (Encoding(UTF8))
import Node.Buffer (create)
import Node.Process (chdir)
import Node.ReadLine (Interface, prompt, close)
--import Node.ChildProcess (spawn, exec)

eval :: EnvRef -> Interface -> Command -> IO Interface
eval e i c = case c of

       Cd (Absolute p) -> do

         pExists <- existsPath p
         if pExists
            then let succ = do
                       modifySTRef e $ (_ {cwd = canonicalize p}) <<< modhistory c
                       prettyLogContents [] p
                  in release (chdir $ showFsPath p) printErr (const succ)
            else log $ "Directory " <> showFsPath p <> " does not exist."
         prompt i

       Cd (Relative p) -> do

         env <- readSTRef e
         eval e i <<< Cd <<< Absolute $ env.cwd </> p

       Ls flags (Relative p) -> do

         env <- readSTRef e
         eval e i <<< Ls flags <<< Absolute $ env.cwd </> p

       Ls flags (Absolute p) -> do

         pExists <- existsPath p
         if pExists
            then prettyLogContents flags p
            else log "Directory does not exist."
         prompt i

       Cat (Right p) -> do

         env <- readSTRef e
         eval e i <<< Cat <<< Left $ env.cwd </> p

       Cat (Left p) -> release (readTextFile UTF8 $ showFsPath p) printErr log *> prompt i

       Pwd -> do

         env <- readSTRef e
         log $ unsafePrintPath env.cwd
         historize e Pwd
         prompt i

       Touch (Right f) -> do

         env <- readSTRef e
         eval e i <<< Touch <<< Left $ env.cwd </> f

       Touch (Left f) -> do

         fileBuffer <- create 0
         release (writeFile (showFsPath f) fileBuffer) printErr unitize
         prompt i

       MkDir (Absolute p) -> release (mkdir $ showFsPath p) printErr unitize *> prompt i

       MkDir (Relative p) -> do

         env <- readSTRef e
         eval e i <<< MkDir <<< Absolute $ env.cwd </> p

       Echo s -> log s *> historize e c *> prompt i

       Rm (Left (Absolute p)) -> release (rmdir $ showFsPath p) printErr unitize *> prompt i

       Rm (Left (Relative p)) -> do

         env <- readSTRef e
         eval e i <<< Rm <<< Left <<< Absolute $ env.cwd </> p

       Rm (Right (Left absfile)) -> release (unlink $ showFsPath absfile) printErr unitize *> prompt i

       Rm (Right (Right relfile)) -> do

         env <- readSTRef e
         eval e i <<< Rm <<< Right <<< Left $ env.cwd </> relfile

       Whoami -> prompt i <* do

         env <- readSTRef e
         log env.whoami
         historize e Whoami

       Exit -> close i

       _ -> log "I don't understand that command." *> prompt i

historize :: EnvRef -> Command -> IO Unit
historize e = void <<< modifySTRef e <<< modhistory

modhistory :: Command -> Env -> Env
modhistory c env = env { history = c : env.history }

existsPath :: Directory -> IO Boolean
existsPath = exists <<< showFsPath

ls :: Array LsFlag -> Directory -> IO DirContents
ls [LsAll] d =
  let onFail _ = pure { dirs: Nil, files: Nil }
      onSucc = partitionDirs <<< map (d </> _) <<< mapMaybe parseRelFile <<< fromFoldable
   in release (readdir $ showFsPath d) onFail onSucc

ls [] d = do
  all <- ls [LsAll] d

  let dirs = map ((d </> _) <<< ) mapMaybe (f <<< runFileName <<< fileName) all.dirs -- FIXME
      files = mapMaybe (f <<< runFileName <<< fileName) all.files

  pure { dirs, files }

    where
      f :: String -> Maybe String
      f str = case charAt 0 str of
                   Just '.' -> Nothing
                   _ -> Just str

ls _ _ = pure { dirs: Nil, files: Nil }

partitionDirs :: List AbsoFile -> IO DirContents
partitionDirs xs = do
  statRes <- catch $ traverse stat xs
  case statRes of
       Right stats ->
         let zipped = zip stats xs
             f acc d = if isDirectory (fst d)
                          then acc { dirs = snd d : acc.dirs }
                          else acc { files = snd d : acc.files }
          in pure $ foldl f { dirs: Nil, files: Nil } zipped
       _ -> pure { dirs: Nil, files: Nil }

prettyLogContents :: Array LsFlag -> Directory -> IO Unit
prettyLogContents fs p = do
  dirFiles <- ls fs p
  let boldDirs = bolden $ intercalate " " (map (_ <> "/") dirFiles.dirs)
      nonboldFiles = intercalate " " dirFiles.files
  when (not $ null dirFiles.dirs) $ log boldDirs
  when (not $ null dirFiles.files) $ log nonboldFiles
