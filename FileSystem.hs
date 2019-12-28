{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}


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
  splitCommand <- return (splitByDelimeter command ' ')
  s' <- magic (path, fs) splitCommand 
  loop s'

serialise :: FileSystem -> IO ()
serialise fs = writeFile "test.txt" (show fs)

deserialise :: IO FileSystem 
deserialise = read <$> (readFile "test.txt")  

splitByDelimeter :: String -> Char -> [String]
splitByDelimeter str del = splitHelper str del [] 
  where splitHelper :: String -> Char -> [String] -> [String]
        splitHelper "" _ res = reverse res
        splitHelper s d res = splitHelper rest d (takeWhile (\x -> x /= del) s : res) 
          where rest' = dropWhile (\x -> x /= del) s
                rest = if null rest' then [] else (tail rest')

filterPath :: [String] -> [String]
filterPath path = filterPathHelper path []
  where filterPathHelper :: [String] -> [String] -> [String]
        filterPathHelper [] res = reverse res
        filterPathHelper [x] res = filterPathHelper [] (x: res)
        filterPathHelper ("":"..":xs) res = filterPathHelper xs ("":res)
        filterPathHelper (_:"..":xs) res = filterPathHelper xs res
        filterPathHelper (x:xs) res = filterPathHelper xs (x:res)

mix :: String -> String -> IO [String]
mix wdir arg = do
  (x:argpath) <- return (splitByDelimeter arg '/')
  if (x == "") then do
    return (filterPath (x:argpath))
  else do
    currpath <- return (splitByDelimeter wdir '/')
    return (filterPath (currpath ++ (x:argpath)))
  

magic :: (String, FileSystem) -> [String] -> IO (String, FileSystem)
magic s [] = do
  return s
magic s ("pwd":args) = do
  pwd s
magic (wdir, fs) ["ls"] = do
  (x:currpath) <- return (splitByDelimeter wdir '/')
  search (wdir, fs) [] (["/"] ++ currpath) ls (Just fs)
magic (wdir, fs) ["ls", arg] = do 
  (x:filteredPath) <- mix wdir arg
  search (wdir, fs) arg (["/"] ++ filteredPath) ls (Just fs)
magic (_, fs) ["cd"] = do
  return ("/", fs)
magic (wdir, fs) ["cd", arg] = do
  (x:filteredPath) <- mix wdir arg 
  search (wdir, fs) ((intercalate "/" (x:filteredPath)) ++ "/")  (["/"] ++ filteredPath) cd (Just fs)
magic s ["cat"] = do
  x <- getLine 
  go x
  where go "." = return s
        go str = do
          putStrLn str 
          str <- getLine
          go str 
magic (wdir, fs) ["cat", arg] = do
  (x:filteredPath) <- mix wdir arg
  search (wdir, fs) ((intercalate "/" (x:filteredPath)) ++ "/") (["/"] ++ filteredPath) cat (Just fs) 
magic (wdir, fs) ("cat":arg:args) = do
  (x:filteredPath) <- mix wdir arg
  s' <- search (wdir, fs) ((intercalate "/" (x:filteredPath)) ++ "/") (["/"] ++ filteredPath) cat (Just fs)
  magic s' ("cat":args)
magic s ["rm"] = do
  putStrLn "rm: missing operand"
  return s
magic (wdir, fs) ["rm", arg] = do 
  (x:filteredPath) <- mix wdir arg
  search (wdir, fs) (intercalate "/" (x:filteredPath)) (["/"] ++ filteredPath) rm (Just fs)
magic (wdir, fs) ("rm":arg:args) = do
  (x:filteredPath) <- mix wdir arg
  s' <- search (wdir, fs) (intercalate "/" (x:filteredPath)) (["/"] ++ filteredPath) rm (Just fs)
  magic s' ("rm":args)   

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

cat :: (Path, FileSystem) -> String -> FileSystem -> IO (Path, FileSystem)
cat s msg (Directory dir) = putStr ("cat: \'" ++ msg ++ "\': Is a directory\n") >> return s
cat s _ (Regular file) = putStr ((fileContent file) ++ "\n") >> return s

rm :: (Path, FileSystem) -> String -> FileSystem -> IO (Path, FileSystem)
rm s msg (Directory dir) = putStr ("rm: cannot remove \'" ++ msg ++ "\': is directory") >> return s
rm (wdir, fs) msg (Regular file) = return (wdir, fs')
  where (x:path) = splitByDelimeter msg '/' 
        fs' = copyFrom fs path 
        copyFrom :: FileSystem -> [String] -> FileSystem
        copyFrom (Directory d) [file] = Directory (Dir (dirName d) (deleteFile file (dirContent d)))
        copyFrom (Regular f) _ = Regular f
        copyFrom (Directory (Dir name [])) _ = Directory (Dir name [])
        copyFrom (Directory (Dir name content)) (x:xs) = Directory (Dir name (copyDirContent content (x:xs)))
        copyDirContent :: [FileSystem] -> [String] -> [FileSystem]
        copyDirContent [Regular f] (y:ys) = [copyFrom (Regular f) (y:ys)]
        copyDirContent [Directory d] (y:ys) = if dirName d == y then [copyFrom (Directory d) ys] else [copyFrom (Directory d) (y:ys)] 
        copyDirContent (Regular f : cont) (y:ys) = copyFrom (Regular f) (y:ys) : copyDirContent cont (y:ys) 
        copyDirContent (Directory d : cont) (y:ys)
          | dirName d == y = copyFrom (Directory d) ys : copyDirContent cont (y:ys)
          | otherwise = copyFrom (Directory d) (y:ys) : copyDirContent cont (y:ys)
        deleteFile :: String -> [FileSystem] -> [FileSystem]
        deleteFile _ [] = []
        deleteFile name (Regular f : cont) 
          | name == fileName f = deleteFile name cont 
          | otherwise = Regular f : deleteFile name cont 
        deleteFile name (Directory d : cont) = Directory d : deleteFile name cont 

-- using these for testing
--
-- file = RegularFile "file1.txt" "Hello" 
-- file2 = RegularFile "file2.txt" "Bye!"
-- root = Directory (Dir "/" [Directory (Dir "home" [Regular file, Directory (Dir "etc" [Regular file2])]), Regular file2]) 
