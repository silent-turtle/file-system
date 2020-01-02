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

main :: IO ()
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
  let splitCommand = splitByDelimeter command ' '
  s' <- magic (path, fs) splitCommand 
  loop s'

serialise :: FileSystem -> IO ()
serialise fs = writeFile "test.txt" (show fs)

deserialise :: IO FileSystem 
deserialise = read <$> readFile "test.txt"  

splitByDelimeter :: String -> Char -> [String]
splitByDelimeter str del = splitHelper str del [] 
  where splitHelper :: String -> Char -> [String] -> [String]
        splitHelper "" _ res = reverse res
        splitHelper s d res = splitHelper rest d (takeWhile (/= del) s : res) 
          where rest' = dropWhile (/= del) s
                rest = if null rest' then [] else tail rest'

filterPath :: [String] -> [String]
filterPath path = filterPathHelper path []
  where filterPathHelper :: [String] -> [String] -> [String]
        filterPathHelper [] res = reverse res
        filterPathHelper [x] res = filterPathHelper [] (x:res)
        filterPathHelper ("" : ".." : xs) res = filterPathHelper xs ("":res)
        filterPathHelper (_ : ".." : xs) res = filterPathHelper xs res
        filterPathHelper (x:xs) res = filterPathHelper xs (x:res)

mix :: String -> String -> IO [String]
mix wdir arg = do
  (x:argpath) <- return (splitByDelimeter arg '/')
  if x == "" then 
    return (filterPath (x : argpath))
  else do
    let currpath = splitByDelimeter wdir '/'
    return (filterPath (currpath ++ (x : argpath)))
  

magic :: (String, FileSystem) -> [String] -> IO (String, FileSystem)
magic s [] = return s
magic s ("pwd":_) = pwd s
magic (wdir, fs) ["ls"] = do
  (_:currpath) <- return (splitByDelimeter wdir '/')
  (res,fs') <- search fs [] ("/" : currpath) ls (Just fs)
  putStr res
  return (wdir, fs')
magic (wdir, fs) ["ls", arg] = do 
  (_:filteredPath) <- mix wdir arg
  (res, fs') <- search fs arg ("/" : filteredPath) ls (Just fs)
  putStr res
  return (wdir, fs') 
magic (_, fs) ["cd"] = return ("/", fs)
magic (wdir, fs) ["cd", arg] = do
  (x:filteredPath) <- mix wdir arg 
  (wdir', fs') <-search fs (intercalate "/" (x:filteredPath) ++ "/")  ("/" : filteredPath) cd (Just fs)
  if null wdir' then
    return (wdir, fs')
  else
    return (wdir', fs')
magic s ["cat"] = do
  x <- getLine 
  go x
  where go "." = return s
        go str = do
          putStrLn str 
          str' <- getLine
          go str' 
magic (wdir, fs) ("cat":arg:args) = do
  go [] (arg:args)
  where go :: String -> [String] -> IO (String, FileSystem) 
        go content [] = putStrLn content >> return (wdir, fs)
        go content [">", file] = do
          (_:fpath) <- mix wdir file
          return (wdir, fCopy fs content fpath catToFile)
        go _ (">":_) = do
          putStrLn "error: should have only one output file"
          return (wdir, fs)
        go content (x:xs) = do
          (y:path) <- mix wdir x 
          (res, _) <- search fs (intercalate "/" (y : path) ++ "/") ("/" : path) getContent (Just fs)
          go (content ++ res ++ "\n") xs 

magic s ["rm"] = do
  putStrLn "rm: missing operand"
  return s
magic (wdir, fs) ["rm", arg] = do 
  (x:filteredPath) <- mix wdir arg
  (res, fs') <- search fs (intercalate "/" (x:filteredPath)) ("/" : filteredPath) rm (Just fs)
  putStr res
  return (wdir, fs')
magic (wdir, fs) ("rm":arg:args) = do
  (x:filteredPath) <- mix wdir arg
  (res, fs') <- search fs (intercalate "/" (x:filteredPath)) ("/" : filteredPath) rm (Just fs)
  putStr res
  magic (wdir, fs') ("rm":args)   
magic s (x:_) = do
  putStrLn (x ++ ": command not found")
  return s 

search :: FileSystem -> String -> [String] -> (FileSystem -> String -> FileSystem -> IO (String, FileSystem)) -> Maybe FileSystem -> IO (Path, FileSystem)
search fs msg [] _ _ = putStr ("Cannot access \'" ++ msg ++ "\': No such file or directory\n") >> return ([], fs)
search fs msg _ _ Nothing = putStr ("Cannot access \'" ++ msg ++ "\': No such file or directory\n") >> return ([], fs)
search fs msg [x] f (Just (Regular file))
  | x == fileName file = f fs msg (Regular file)
  | otherwise = search fs msg [] f Nothing
search fs msg [x] f (Just (Directory dir))
  | x == dirName dir = f fs msg (Directory dir)
  | otherwise = search fs msg [] f Nothing
search fs msg (_:_:_) _ (Just (Regular _)) = putStr ("Cannot access \'" ++ msg ++ "\': No such file or directory\n") >> return ([], fs)
search fs msg (x:y:xs) f (Just (Directory dir))
  | x == dirName dir = search fs msg (y:xs) f (find' y (dirContent dir))
  | otherwise = search fs msg [] f Nothing
  where find' _ [] = Nothing
        find' str (Regular file : dirs)
          | fileName file == str = Just (Regular file)
          | otherwise = find' str dirs
        find' str (Directory d : dirs)
          | dirName d == str = Just (Directory d)
          | otherwise = find' str dirs


pwd :: (Path, FileSystem) -> IO (Path, FileSystem)
pwd (str, fs) = putStr (str ++ "\n") >> return (str, fs)

ls :: FileSystem -> String -> FileSystem -> IO (String, FileSystem)
ls fs _ (Regular file) = return (fileName file ++ "\n", fs)
ls fs _ (Directory dir) = return (printAll (dirContent dir), fs)
  where printAll [] = "\n"
        printAll (Regular x : content) = fileName x ++ "  " ++ printAll content
        printAll (Directory d : content) = dirName d ++ "  " ++ printAll content


cd :: FileSystem -> String -> FileSystem -> IO (String, FileSystem)
cd fs newpath (Regular _) = putStr ("cd: \'" ++ newpath ++ "\': Not a directory\n") >> return ([], fs)
cd fs newpath (Directory _) = return (newpath, fs) 

getContent :: FileSystem -> String -> FileSystem -> IO (String, FileSystem)
getContent fs msg (Directory _) = putStr ("cat: \'" ++ msg ++ "\': Is a directory\n") >> return ([], fs)
getContent fs _ (Regular file) = return (fileContent file, fs)

catToFile :: String -> String -> [FileSystem] -> [FileSystem] 
catToFile content name [] = [Regular (RegularFile name content)]
catToFile content name [Regular f] 
  | fileName f == name = [Regular (RegularFile name content)] 
  | otherwise = [Regular f, Regular (RegularFile name content)]
catToFile content name [Directory d] = [Directory d, Regular (RegularFile name content)]
catToFile content name (Regular f : xs) 
  | fileName f == name = Regular (RegularFile name content) : xs 
  | otherwise = Regular f : catToFile content name xs
catToFile content name (Directory d : xs) = Directory d : catToFile content name xs  

rm :: FileSystem -> String -> FileSystem -> IO (String, FileSystem)
rm fs msg (Directory _) = putStr ("rm: cannot remove \'" ++ msg ++ "\': is directory\n") >> return ([], fs)
rm fs msg (Regular _) = return ([], fs')
  where (_:path) = splitByDelimeter msg '/' 
        fs' = fCopy fs [] path deleteFile 
        deleteFile :: String -> String -> [FileSystem] -> [FileSystem]
        deleteFile _ _ [] = []
        deleteFile name x (Regular f : cont) 
          | name == fileName f = deleteFile name x cont 
          | otherwise = Regular f : deleteFile name x cont 
        deleteFile name x (Directory d : cont) = Directory d : deleteFile name x cont 

fCopy :: FileSystem -> String -> [String] -> (String -> String -> [FileSystem] -> [FileSystem]) -> FileSystem
fCopy (Regular f) _ _ _ = Regular f
fCopy (Directory (Dir name [])) _ _ _ = Directory (Dir name [])
fCopy (Directory d) _ [] _ = Directory d
fCopy (Directory d) content [filename] g = Directory (Dir (dirName d) (g content filename (dirContent d)))
fCopy (Directory (Dir name dircont)) content (x:xs) g = Directory (Dir name (copyDirContent dircont (x:xs)))
  where copyDirContent :: [FileSystem] -> [String] -> [FileSystem]
        copyDirContent [] _ = []
        copyDirContent [Regular f] _ = [Regular f]
        copyDirContent [Directory d] [] = [Directory d]
        copyDirContent (el:rest) [] = el : copyDirContent rest []  
        copyDirContent [Directory d] (y:ys) 
          | dirName d == y = [fCopy (Directory d) content ys g] 
          | otherwise = [fCopy (Directory d) content (y:ys) g]
        copyDirContent (Regular f : cont) (y:ys) = fCopy (Regular f) content (y:ys) g : copyDirContent cont (y:ys)
        copyDirContent (Directory d : cont) (y:ys) 
          | dirName d == y = fCopy (Directory d) content ys g : copyDirContent cont (y:ys) 
          | otherwise = fCopy (Directory d) content (y:ys) g : copyDirContent cont (y:ys)
        

-- using these for testing
--
-- file = RegularFile "file1.txt" "Hello" 
-- file2 = RegularFile "file2.txt" "Bye!"
-- root = Directory (Dir "/" [Directory (Dir "home" [Regular file, Directory (Dir "etc" [Regular file2])]), Regular file2]) 
