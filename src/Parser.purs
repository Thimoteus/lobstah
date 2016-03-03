module Parser where

import Prelude
import Types (Command(..), Dir(..), LsFlag(..), Env)
import Util (dotSlash, showFsPath)

import Data.Either (Either(..))
import Data.Functor ((<$))
import Data.Maybe(Maybe(..))
import Data.Path.Pathy (parseAbsDir, parseRelDir, parseAbsFile, parseRelFile)
import Data.String (replace)

import Control.Apply ((*>))

import Text.Parsing.Simple
import Text.Parsing.Combinators (bracket, option)

parseCommand :: String -> Env -> Either String Command
parseCommand cmd env = parse (commandParser env) cmd

commandParser :: Env -> Parser Command
commandParser env = do

  skipSpaces
  cmd <- lookahead word

  case cmd of

       "cd" -> cd env <?> "Incorrect `cd` syntax. Did you forget to add a slash?"
       "echo" -> echo <?> "Incorrect `echo` syntax."
       "pwd" -> pwd <?> "Incorrect `pwd` syntax."
       "touch" -> touch env <?> "Incorrect `touch` syntax."
       "ls" -> ls env <?> "Incorrect `ls` syntax."
       "cat" -> cat env <?> "Incorrect `cat` syntax."
       "exit" -> exit <?> "Incorrect `exit` syntax."
       "whoami" -> whoami <?> "Incorrect `whoami` syntax."
       "mkdir" -> mkdir env <?> "Incorrect `mkdir` syntax. Did you forget to add a slash?"
       "rm" -> rm env <?> "Incorrect `rm` syntax."
       _ -> external

cd :: Env -> Parser Command
cd env = Cd <$> do

  string "cd"
  skipSpaces
  dir <- substitute env <<< fromCharList <$> many item

  case dir of
       "" -> pure $ Absolute env.home
       _ -> case parseAbsDir dir of
                 Just x -> pure (Absolute x)
                 _ -> case parseRelDir dir of
                           Just y -> pure (Relative y)
                           _ -> fail "Argument must be a path"

echo :: Parser Command
echo = Echo <$> do

  string "echo"
  skipSpaces
  msg <- bracket (char '"') (fromCharList <$> many (sat (_ /= '"'))) (char '"')

  pure msg

pwd :: Parser Command
pwd = Pwd <$ string "pwd"

mkdir :: Env -> Parser Command
mkdir env = MkDir <$> do

  string "mkdir"
  skipSpaces
  dir <- substitute env <<< fromCharList <$> many item
  
  case parseRelDir dir of
       Just y -> pure (Relative y)
       _ -> case parseAbsDir dir of
                 Just x -> pure (Absolute x)
                 _ -> fail "Argument must be a path"

whoami :: Parser Command
whoami = Whoami <$ string "whoami"

exit :: Parser Command
exit = Exit <$ string "exit"

rm :: Env -> Parser Command
rm env = Rm <$> do

  string "rm"
  skipSpaces
  thing <- substitute env <<< fromCharList <$> many item

  case parseRelDir thing of
       Just y -> pure $ Left $ Relative y
       _ -> case parseAbsDir thing of
                 Just x -> pure $ Left $ Absolute x
                 _ -> case parseRelFile thing of
                           Just z -> pure $ Right $ Right z
                           _ -> case parseAbsFile thing of
                                     Just omega -> pure $ Right $ Left omega
                                     _ -> fail "Argument must be a path or file."

cat :: Env -> Parser Command
cat env = Cat <$> do

  string "cat"
  skipSpaces
  file <- substitute env <<< fromCharList <$> many item

  case parseRelFile file of
       Just x -> pure (Right x)
       _ -> case parseAbsFile file of
                 Just y -> pure (Left y)
                 _ -> fail $ "Could not cat contents of " <> file

touch :: Env -> Parser Command
touch env = Touch <$> do

  string "touch"
  skipSpaces
  file <- substitute env <<< fromCharList <$> many item

  case parseRelFile file of
       Just x -> pure (Right x)
       _ -> case parseAbsFile file of
                 Just y -> pure (Left y)
                 _ -> fail $ "Could not create file " <> file

ls :: Env -> Parser Command
ls env = do

  string "ls"
  skipSpaces
  flags <- option [] $ string "-a" *> pure [LsAll]
  skipSpaces
  dir <- substitute env <<< fromCharList <$> many item

  case dir of
       "" -> pure <<< Ls flags $ Relative dotSlash
       _ -> case parseRelDir dir of
                 Just x -> pure <<< Ls flags $ Relative x
                 _ -> case parseAbsDir dir of
                           Just y -> pure <<< Ls flags $ Absolute y
                           _ -> fail $ dir <> " is not a directory."

external :: Parser Command
external = Exec <<< fromCharList <$> many item

quoted :: Parser String
quoted = fromCharList <$> do
  char '"'
  s <- many $ sat (_ /= '"')
  char '"'
  pure s

substitute :: Env -> String -> String
substitute env s = replace "~/" (showFsPath env.home) s

