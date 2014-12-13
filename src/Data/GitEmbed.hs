{-# LANGUAGE TemplateHaskell #-}

-- | Use Template Haskell to embed Git revision, branch, and tag information.
--
-- Also adds dependent files so that changes to git refs cause a rebuild.
--
-- Example:
--
-- > {-# LANGUAGE TemplateHaskell #-}
-- > import Data.GitEmbed
-- >
-- > main :: IO ()
-- > main = putStrLn ("Git revision: " ++ gitRev ++ ", branch: " ++ gitBranch)
-- >
-- > gitRev :: String
-- > gitRev = $(embedGitShortRevision)
-- >
-- > gitBranch :: String
-- > gitBranch = $(embedGitBranch)
module Data.GitEmbed
    ( embedGitRevision
    , embedGitShortRevision
    , embedGitBranch
    , embedGitDescribe
    , embedGit )
    where

import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Language.Haskell.TH.Syntax
    ( Exp (LitE)
    , Lit (StringL)
    , Q
    , Quasi(qAddDependentFile)
    , runIO )
import System.Directory (getDirectoryContents, doesDirectoryExist)
import System.Environment (lookupEnv)
import System.FilePath (combine)
import System.Process (readProcess)

-- | Embed the current Git long hexadecimal revision ID.
embedGitRevision :: Q Exp
embedGitRevision = embedGit ["rev-parse", "HEAD"]

-- | Embed the current Git short hexadecimal revision ID (first 7 digits).
embedGitShortRevision :: Q Exp
embedGitShortRevision = embedGit ["rev-parse", "--short", "HEAD"]

-- | Embed the current Git branch name.
embedGitBranch :: Q Exp
embedGitBranch = embedGit ["rev-parse", "--abbrev-ref", "HEAD"]

-- | Embed output of @git describe@.
embedGitDescribe :: [String] -- ^ Arguments to pass to @git describe@ command.
                             -- Run @git help describe@ for documentation
                             -- of options.
                 -> Q Exp
embedGitDescribe args = embedGit ("describe" : args)

-- | Embed output of any Git command.
embedGit :: [String] -- ^ Arguments to pass to @git@ command.
                     -- Run @git help@ for documentation.
         -> Q Exp
embedGit args = do
    addRefDependentFiles
    gitOut <- runIO (readProcess "git" args "")
    return $! LitE (StringL (dropWhileEnd isSpace gitOut))

-- | Use 'qAddDependentFile' on all files under @.git/refs@.
addRefDependentFiles :: Q ()
addRefDependentFiles = do
    gitDir <- runIO findGitDir
    qAddDependentFile (combine gitDir "HEAD")
    addDirDeps (combine gitDir "refs")
  where
    findGitDir = do
        maybeGitDir <- lookupEnv "GIT_DIR"
        case maybeGitDir of
            Just dir -> return dir
            Nothing -> fmap (\x -> combine (dropWhileEnd isSpace x) ".git")
                            (readProcess "git" ["rev-parse", "--show-cdup"] "")
    addDirDeps dir = do
        subPaths <- runIO (fmap (map (combine dir) . filter notHidden)
                                (getDirectoryContents dir))
        mapM_ recursePath subPaths
    recursePath path = do
        isDir <- runIO (doesDirectoryExist path)
        if isDir
            then addDirDeps path
            else qAddDependentFile path
    notHidden ('.':_) = False
    notHidden _ = True
