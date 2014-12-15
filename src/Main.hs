module Main where

import Control.Monad
import Crypto.Hash.SHA256 (hash)
import Data.ByteString as B (ByteString, readFile)
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import Data.Ord
import Options.Applicative
import Safe
import System.Directory
import System.Environment
import System.FilePath.Find as Find
import System.IO
import System.Posix as P

data Options = Options { path :: FilePath
                       , recursive :: Bool
                       , delete :: Bool
                       }

main :: IO ()
main = execParser (info parser mempty) >>= oldMain

parser = Options <$> argument str (metavar "PATH")
                 <*> switch (short 'r' <>
                             long  "recursive" <>
                             help  "Traverse all subfolders")
                 <*> switch (short 'd' <>
                             long  "delete" <>
                             help  "Delete all duplicates")

oldMain :: Options -> IO ()
oldMain o = do
  dupes <- dups o
  let keepers = map (keepWhich compare) dupes
      goners = concatMap snd keepers
  mapM_ putStrLn goners


dups :: Options -> IO [[FilePath]]
dups o = do
  sizeMap <- Find.fold recPredicate buildMap M.empty (path o)
  let possibleDupes = M.filter (\xs -> length xs > 1) sizeMap
  groups <- concatMapM checkHashes (map snd $ M.toList possibleDupes)
  return (filter (\ls -> length ls > 1) groups)
  where recPredicate = return (recursive o)

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

keepWhich :: (FilePath -> FilePath -> Ordering)
          -> [FilePath] -> (FilePath, [FilePath])
keepWhich ord xs = let keeper = maximumBy ord xs
                   in (keeper, filter (/= keeper) xs)

hashEm :: [FilePath] -> IO [(ByteString, FilePath)]
hashEm xs = liftM (`zip` xs) (mapM hashFile xs)

hashFile :: FilePath -> IO ByteString
hashFile fn = liftM hash (B.readFile fn)

concatMapM :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = liftM concat (mapM f xs)
