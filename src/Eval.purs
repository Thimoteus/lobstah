module Eval where

import Prelude
import Types (IO, EnvRef, Env, Directory, Command(..), Dir(..), LsFlag(..), DirContents, AbsoFile, emptyDirContents)
import Util (showFsPath, bolden, release, printErr, unitize, catch)

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.List (List(), (:), mapMaybe, null, fromFoldable, zip, fromList)
import Data.Path.Pathy ((</>), unsafePrintPath, canonicalize, fileName, runFileName, parseRelFile)
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
                       prettyLogContents [] p >>= modcompletions e
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
            then prettyLogContents flags p >>= modcompletions e
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
  let onFail _ = pure emptyDirContents
      onSucc = partitionDirs <<< map (d </> _) <<< mapMaybe parseRelFile <<< fromFoldable
   in release (readdir $ showFsPath d) onFail onSucc

ls [] d = do
  all <- ls [LsAll] d

  let stripDir = map (d </> _) <<< mapMaybe parseRelFile <<< mapMaybe (hiddenFile <<< runFileName <<< fileName)
      dirs = stripDir all.dirs
      files = stripDir all.files

  pure { dirs, files }

ls _ _ = pure emptyDirContents

hiddenFile :: String -> Maybe String
hiddenFile str =
  case charAt 0 str of
      Just '.' -> Nothing
      _ -> Just str

partitionDirs :: List AbsoFile -> IO DirContents
partitionDirs xs = do
  let stringified = map showFsPath xs
  statRes <- catch $ traverse stat stringified
  case statRes of
       Right stats ->
         let zipped = zip stats xs
             f acc d = if isDirectory (fst d)
                          then acc { dirs = snd d : acc.dirs }
                          else acc { files = snd d : acc.files }
          in pure $ foldl f emptyDirContents zipped
       _ -> pure emptyDirContents

prettyLogContents :: Array LsFlag -> Directory -> IO DirContents
prettyLogContents fs p = do
  dirFiles <- ls fs p
  let dirs = map (runFileName <<< fileName) dirFiles.dirs
      files = map (runFileName <<< fileName) dirFiles.files
      boldDirs = bolden $ intercalate " " (map (_ <> "/") dirs)
      nonboldFiles = intercalate " " files
  when (not $ null dirFiles.dirs) $ log boldDirs
  when (not $ null dirFiles.files) $ log nonboldFiles
  pure dirFiles

modcompletions :: EnvRef -> DirContents -> IO Unit
modcompletions e { files, dirs } = do
  let dirs' = map ((_ <> "/") <<< runFileName <<< fileName) dirs
      files' = map (runFileName <<< fileName) files
      f env = env { completions = fromList files' ++ fromList dirs' }
  modifySTRef e f
  pure unit
