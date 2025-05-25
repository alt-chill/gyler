module Main where

import Options.Applicative
import System.Environment (getArgs)
import Data.List (isPrefixOf)
import System.Process

getTaskIds :: IO [String]
getTaskIds = do
    output <- readProcess "ssh" ["gyle", "task", "ls"] ""
    return $ takeWhile (/=' ') . drop 1 <$> lines output

taskIdCompleter :: Completer
taskIdCompleter = mkCompleter $ \input -> do
    filter (input `isPrefixOf`) <$> getTaskIds

data Command
  = Run !String !String  -- task_id subtask_id
  | Rebuild !String      -- source_package

main :: IO ()
main = do
  cmd <- execParser opts
  case cmd of
    Run taskId subtaskId   -> putStrLn $ "Running task " ++ taskId ++ ", subtask " ++ subtaskId
    Rebuild source_package -> putStrLn $ "Rebuilding package " ++ source_package

opts :: ParserInfo Command
opts = info (commandParser <**> helper)
  ( fullDesc <> progDesc "Run tasks with subtasks" )

commandParser :: Parser Command
commandParser = hsubparser
  ( command "run"     (info runParser (progDesc "Run a task"))
 <> command "rebuild" (info rebuildParser (progDesc "Rebuild package"))
  )

runParser :: Parser Command
runParser = Run
  <$> argument str
        ( metavar "TASK_ID"
       <> completer taskIdCompleter )
  <*> argument str
        ( metavar "SUBTASK_ID"
       <> completer mkCompleterWithTask )

mkCompleterWithTask :: Completer
mkCompleterWithTask = mkCompleter $ \_ -> do
  args <- getArgs
  let filtered = filter (/="--bash-completion-word") . dropWhile (/= "run") $ args
  case filtered of
    _ : taskId : _ -> getSubtasksFor taskId
    _anyOther -> pure []

getSubtasksFor :: String -> IO [String]
getSubtasksFor "task1" = pure ["sub1", "sub2"]
getSubtasksFor "task2" = pure ["sub3", "sub4"]
getSubtasksFor _       = pure []

rebuildParser :: Parser Command
rebuildParser = Rebuild
  <$> argument str
        ( metavar "PACKAGE"
       <> completer (listIOCompleter getPackagesInRepo))

getPackagesInRepo :: IO [String]
getPackagesInRepo = lines <$> readFile "pkg_list_sisyphus"
