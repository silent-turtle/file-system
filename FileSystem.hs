{-# LANGUAGE TypeApplications #-}

import Data.List

type Path = String
data RegularFile = RegularFile { fileName :: String, fileContent :: String } deriving (Show, Read)
data Dir = Dir { dirName :: String, dirContent :: [FileSystem] } deriving (Show, Read)

data FileSystem 
  = Regular RegularFile 
  | Directory Dir
  deriving (Show, Read)

main = do
  f <- deserialise
  (dir, fs) <- createPair "/" f 
  (_, fs') <- loop (dir, fs)
  serialise fs'

createPair :: Path -> FileSystem -> IO (Path, FileSystem)
createPair dir fs = return (dir, fs)

loop :: (Path, FileSystem) -> IO (Path, FileSystem)
loop (path, fs) = do
  putStr (path ++ "$ ")
  command <- getLine
  splitCommand <- splitByDelimeter command ' '
  s' <- magic (path, fs) splitCommand 
  loop s'

serialise :: FileSystem -> IO ()
serialise fs = writeFile "test.txt" (show fs)

deserialise :: IO FileSystem 
deserialise = read <$> (readFile "test.txt")  

splitByDelimeter :: String -> Char -> IO [String]
splitByDelimeter str del = return (splitHelper str del []) 
  where splitHelper :: String -> Char -> [String] -> [String]
        splitHelper "" _ res = reverse res
        splitHelper s d res = splitHelper rest d (takeWhile (\x -> x /= del) s : res) 
          where rest' = dropWhile (\x -> x /= del) s
                rest = if null rest' then [] else (tail rest')

filterPath :: [String] -> IO [String]
filterPath path = return (filterPathHelper path [])
  where filterPathHelper :: [String] -> [String] -> [String]
        filterPathHelper [] res = reverse res
        filterPathHelper [x] res = filterPathHelper [] (x: res)
        filterPathHelper ("":"..":xs) res = filterPathHelper xs ("":res)
        filterPathHelper (_:"..":xs) res = filterPathHelper xs res
        filterPathHelper (x:xs) res = filterPathHelper xs (x:res)

mix :: String -> String -> IO [String]
mix wdir arg = do
  (x:argpath) <- splitByDelimeter arg '/'
  if (x == "") then do
    filterPath (x:argpath)
  else do
    currpath <- splitByDelimeter wdir '/'
    filterPath (currpath ++ (x:argpath))
  

magic :: (String, FileSystem) -> [String] -> IO (String, FileSystem)
magic s [] = do
  return s
magic s ("pwd":args) = do
  pwd s
magic (wdir, fs) ["ls"] = do
  (x:currpath) <- splitByDelimeter wdir '/'
  search (wdir, fs) [] (["/"] ++ currpath) ls (Just fs)
magic (wdir, fs) ["ls", arg] = do 
  (z:filteredPath) <- mix wdir arg
  search (wdir, fs) arg (["/"] ++ filteredPath) ls (Just fs)
magic (_, fs) ["cd"] = do
  return ("/", fs)
magic (wdir, fs) ["cd", arg] = do
  (z:filteredPath) <- mix wdir arg 
  search (wdir, fs) ((intercalate "/" (z:filteredPath)) ++ "/")  (["/"] ++ filteredPath) cd (Just fs)

search :: (Path, FileSystem) -> String -> [String] -> ((Path, FileSystem) -> String -> FileSystem -> IO (Path, FileSystem)) -> Maybe FileSystem -> IO (Path, FileSystem)
search s msg _ _ Nothing = putStr ("Cannot access \'" ++ msg ++ "\': No such file or directory\n") >> return s
search s msg [x] f (Just (Regular file))
  | x == fileName file = f s msg (Regular file)
  | otherwise = search s msg [] f Nothing
search s msg [x] f (Just (Directory dir))
  | x == dirName dir = f s msg (Directory dir)
  | otherwise = search s msg [] f Nothing
search s msg (x:y:xs) f (Just (Directory dir))
  | x == dirName dir = search s msg (y:xs) f (find y (dirContent dir))
  | otherwise = search s msg [] f Nothing
  where find str [] = Nothing
        find str (Regular file : dirs)
          | fileName file == str = Just (Regular file)
          | otherwise = find str dirs
        find str (Directory dir : dirs)
          | dirName dir == str = Just (Directory dir)
          | otherwise = find str dirs

pwd :: (Path, FileSystem) -> IO (Path, FileSystem)
pwd (str, fs) = putStr (str ++ "\n") >> return (str, fs)

ls :: (Path, FileSystem) -> String -> FileSystem -> IO (Path, FileSystem)
ls s _ (Regular file) = putStr (fileName file ++ "\n") >> return s
ls s _ (Directory dir) = printAll (dirContent dir) >> return s
  where printAll [] = putStr "\n"
        printAll (Regular x : dir) = putStr (fileName x ++ "  ") >> printAll dir
        printAll (Directory d : dir) = putStr (dirName d ++ "  ") >> printAll dir 

cd :: (Path, FileSystem) -> String -> FileSystem -> IO (Path, FileSystem)
cd (path, fs) newpath (Regular file) = putStr ("cd: \'" ++ newpath ++ "\': Not a directory\n") >> return (path, fs)
cd (_, fs) newpath (Directory dir) = return (newpath, fs) 

-- using these for testing
--
file = RegularFile "file1.txt" "Hello" 
file2 = RegularFile "file2.txt" "Bye!"
root = Directory (Dir "/" [Directory (Dir "home" [Regular file, Directory (Dir "etc" [Regular file2])]), Regular file2]) 
