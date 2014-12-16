module Main (main) where

import Prelude hiding             (catch)

import Control.Applicative        ((<$>), (<*>))
import Control.Exception          (catch, throwIO)
import Control.Monad              (liftM)
import Crypto.Hash.SHA256         (hash)
import Data.ByteString as B       (ByteString, readFile)
import Data.List                  (groupBy)
import Data.Map                   (Map)
import qualified Data.Map as M
import Data.Maybe                 (catMaybes)
import Data.Monoid                ((<>), mempty)
import Options.Applicative
import System.Directory           (removeFile)
import System.FilePath.Find as F
import System.IO                  (putStrLn)
import System.IO.Error
import System.Posix as P          (FileOffset, fileSize)

data Options = Options { path :: FilePath
                       , recursive :: Bool
                       , deleteDupes :: Bool
                       , longer :: Bool
                       , listKeepers :: Bool
                       }

type Hash = ByteString

main :: IO ()
main = execParser (info parser mempty) >>= oldMain

parser = Options <$> argument str (metavar "PATH")
                 <*> switch (short 'r' <>
                             long  "recursive" <>
                             help  "Traverse all subfolders")
                 <*> switch (short 'd' <>
                             long  "delete" <>
                             help  "Delete all duplicates")
                 <*> switch (short 'l' <>
                             long  "longer-names" <>
                             help  "Keep duplicates with longer filenames.\
                                   \Shorter-named duplicates will be listed\
                                   \or deleted.")
                 <*> switch (short 'k' <>
                             long  "list-keepers" <>
                             help  "List the duplicates being kept")

oldMain :: Options -> IO ()
oldMain o = do
  dupes <- dups o
  let fnCompare = if longer o then flip compare else compare
      compares = map (keepWhich fnCompare) dupes
      keepers = catMaybes (map fst compares)
      goners = concatMap snd compares
  case (deleteDupes o, listKeepers o)
    -- Probably not the best way to express this!
    of (True,  True)  -> deleteFiles goners >> listFiles keepers
       (True,  False) -> deleteFiles goners
       (False, True)  -> listFiles keepers
       (False, False) -> listFiles goners

deleteFiles :: [FilePath] -> IO ()
deleteFiles xs = mapM_ removeFile xs `catch` handle
  where handle e
          -- Huge potential for race conditions...
          | isDoesNotExistError e = throwIO e
          | otherwise = return () -- TODO

listFiles :: [FilePath] -> IO ()
listFiles = mapM_ putStrLn

dups :: Options -> IO [[FilePath]]
dups o = do
  sizeMap <- F.fold recPredicate buildMap M.empty (path o)
  let possibleDupes = M.filter ((> 1) . length) sizeMap
  groups <- concatMapM checkHashes (map snd $ M.toList possibleDupes)
  return (filter ((> 1) . length) groups)
  where recPredicate = if recursive o then F.always else once
        once = liftM (== 0) F.depth

buildMap :: Map FileOffset [FilePath] -> FileInfo -> Map FileOffset [FilePath]
buildMap m info = case ftyp of
  RegularFile -> if M.notMember size m
                 then M.insert size [infoPath info] m
                 else M.adjust (infoPath info :) size m
  _           -> m
  where stat = infoStatus info
        size = P.fileSize stat
        ftyp = statusType stat

checkHashes :: [FilePath] -> IO [[FilePath]]
checkHashes xs = do
  hashPairs <- hashEm xs
  let groups = groupBy (\(a,b) (c,d) -> a == c) hashPairs
      snds = map (map snd) groups
  return snds

keepWhich :: (a -> a -> Ordering) -> [a] -> (Maybe a, [a])
keepWhich order xs = go Nothing xs []
  where go Nothing  (x:xs) y = go (Just x) xs y
        go (Just z) (x:xs) y = let (w, r) = case x `order` z of
                                              LT -> (x, z:y)
                                              _  -> (z, x:y)
                               in go (Just w) xs r
        go x [] losers = (x, losers)

hashEm :: [FilePath] -> IO [(Hash, FilePath)]
hashEm xs = liftM (`zip` xs) (mapM hashFile xs)

hashFile :: FilePath -> IO Hash
hashFile fn = liftM hash (B.readFile fn)

concatMapM :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = liftM concat (mapM f xs)
