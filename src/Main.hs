module Main where

import Control.Monad
import Crypto.Hash.SHA256 (hash)
import Data.ByteString as B (ByteString, readFile)
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Ord
import Safe
import System.Directory
import System.Environment
import System.FilePath.Glob
import System.IO
import System.Posix

main = do
  args <- getArgs
  let path = headMay args
  dupes <- maybe (return [[]]) dups path
  let keepers = map (keepWhich compare) dupes
      goners = concatMap snd keepers
  mapM_ putStrLn goners

dups :: FilePath -> IO [[FilePath]]
dups p = do
  ls <- globDir1 (compile "*") p
  files <- filterM doesFileExist ls
  stats <- mapM getFileStatus files
  let sizePairs = zipWith (\a b -> (fileSize a, b)) stats files
      sizeMap = buildMap sizePairs
      possibleDupes = M.filter (\xs -> length xs > 1) sizeMap
  groups <- concatMapM checkHashes (map snd $ M.toList possibleDupes)
  return (filter (\ls -> length ls > 1) groups)

buildMap :: Ord a => [(a, b)] -> Map a [b]
buildMap = go M.empty
  where go m [] = m
        go m ((x,y):xs) = if M.notMember x m
                          then go (M.insert x [y] m) xs
                          else go (M.adjust (y:) x m) xs

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
hashEm xs = mapM hashFile xs >>= (return . (flip zip) xs)

hashFile :: FilePath -> IO ByteString
hashFile fn = B.readFile fn >>= return . hash

concatMapM :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = liftM concat (mapM f xs)
