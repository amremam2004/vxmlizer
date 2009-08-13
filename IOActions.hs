module IOActions(

    putChar,                   -- :: Char   -> IO ()
    putStr,                    -- :: String -> IO ()
    putStrLn,                  -- :: String -> IO ()
    print,                     -- :: Show a => a -> IO ()
    getChar,                   -- :: IO Char
    getLine,                   -- :: IO String
    getContents,               -- :: IO String
    readFile,                  -- :: String -> IO String
    writeFile,                 -- :: String -> IO ()

    FilePath, (</>),
    getDirectoryContents,      -- :: FilePath -> IO [FilePath]
    getDirectoryPaths,         -- :: FilePath -> IO [FilePath]
    getCurrentDirectory,       -- :: IO FilePath
    getHomeDirectory,          -- :: IO FilePath
    doesFileExist,             -- :: FilePath -> IO Bool
    doesDirectoryExist,        -- :: FilePath -> IO Bool
    createDirectory,           -- :: FilePath -> IO ()

    FileSystem(..),            -- :: data FileSystem = ...
    getFileSystem,             -- :: Int -> FilePath -> IO FileSystem

    getFiles,                  -- :: FilePath -> IO [FilePath]
    getDirectories,            -- :: FilePath -> IO [FilePath]

    system,                    -- :: String -> IO ExitCode
    getArgs,                   -- :: IO [String]
    getProgName,               -- :: IO String
    getEnv,                    -- :: String -> IO String

    runCommand,                -- :: String -> FilePath -> IO ExitCode

    inIO,                      -- :: (a -> b) -> a -> IO b
    (>>=),                     -- :: IO a -> (a -> IO b) -> IO b
    return,                    -- :: a -> IO a
    mapM,                      -- :: (a -> IO b) -> [a] -> IO [b]
    mapM_,                     -- :: (a -> IO b) -> [a] -> IO ()
    filterM,                   -- :: (a -> IO Bool) -> [a] -> IO [a]
    foldM,                     -- :: (a -> b -> IO a) -> a -> [b] -> IO a
    foldM_,                    -- :: (a -> b -> IO a) -> a -> [b] -> IO ()
    replicateM,                -- :: Int -> IO a -> IO [a]
    replicateM_,               -- :: Int -> IO a -> IO ()

    module Data.List,          -- :: isPrefixOf, isSuffixOf, ...

	graphviz,                  -- :: LabeledTree t => String -> t -> IO ()

 )  where

import Control.Monad
import System.IO
import System.Cmd
import System.Directory
import System.Environment
import System.Exit
import Data.List

import Treedot

inIO  :: (a -> b) -> a -> IO b
inIO f = return . f

runCommand :: String -> FilePath -> IO ExitCode
runCommand cmd path = system (cmd ++ " " ++ show path)

{-

readFile "tree.dot" >>= inIO (length . lines) >>= print
getCurrentDirectory >>= getDirectoryContents >>= print
getHomeDirectory    >>= getFileSystem 1 >>= inIO toDot >>= writeFile "tree.dot"
getCurrentDirectory >>= inIO (</> "Archive") >>= getFileSystem 3 >>= inIO toDot >>= writeFile "tree.dot"
getCurrentDirectory >>= getFiles >>= inIO sort >>= print
getCurrentDirectory >>= getFiles >>= inIO (filter (isSuffixOf ".lhs")) >>= mapM (\f -> readFile f >>= inIO (length . lines)) >>= inIO maximum >>= print
getCurrentDirectory >>= getFiles >>= inIO (filter (isSuffixOf ".lhs")) >>= mapM (\f -> readFile f >>= inIO (length . lines)) >>= inIO sum >>= print
getCurrentDirectory >>= getFiles >>= inIO (filter (isSuffixOf ".pdf")) >>= inIO head >>= runCommand "open"
getHomeDirectory >>= inIO (</> "local/lib") >>= getFileSystem 2 >>= inIO toDot >>= writeFile "tree.dot" >>= const (runCommand "open -a GraphViz" "tree.dot") 
getCurrentDirectory >>= getFiles >>= inIO (filter (isSuffixOf "hs")) >>= mapM (runCommand "wc")

-}

getFiles        :: FilePath -> IO [FilePath]
getFiles path    = getDirectoryContents path
                   >>= inIO (filter (not . dotFile))
                   >>= filterM doesFileExist

getDirectories     :: FilePath -> IO [FilePath]
getDirectories path = getDirectoryContents path
                      >>= inIO (filter (not . dotFile))
                      >>= filterM doesDirectoryExist

dotFile           :: FilePath -> Bool
dotFile ('.':name) = True
dotFile other      = False

getDirectoryPaths :: FilePath -> IO [FilePath]
getDirectoryPaths path
                   = getDirectoryContents path >>= return . map (path </>)

(</>)  :: FilePath -> FilePath -> FilePath
p </> q = p ++ "/" ++ q

data FileSystem = File FilePath
                | Folder FilePath [FileSystem]
                | Foldep FilePath
                  deriving Show

instance Tree FileSystem where
  subtrees (File path)      = []
  subtrees (Folder name cs) = cs
  subtrees (Foldep path)    = []

instance LabeledTree FileSystem where
  label (File path)      = path
  label (Folder path cs) = path ++ "/"
  label (Foldep path)    = path ++ "..."

getFileSystem :: Int -> FilePath -> IO FileSystem
getFileSystem n path
 = getFileSystemDir n path path

getFileSystemDir :: Int -> FilePath -> FilePath -> IO FileSystem
getFileSystemDir n path name
 | n < 1     = return (Foldep name)
 | otherwise = getDirectoryContents path
               >>= inIO (filter (not . dotFile))
               >>= mapM (getFileSystemIn (n-1) path)
               >>= inIO (Folder name)

getFileSystemIn :: Int -> FilePath -> FilePath -> IO FileSystem
getFileSystemIn n parent child
 = doesDirectoryExist path
   >>= \b-> case b of
        True  -> getFileSystemDir n path child
        False -> return (File child)
   where path = parent </> child

graphviz       :: LabeledTree t => String -> t -> IO ()
graphviz path t = writeFile path (toDot t)
                  >> runCommand "open -a GraphViz" path
                  >> return ()
