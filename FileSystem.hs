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
loop s = do
  putStr "$ "
  command <- getLine
  splitCommand <- splitByDelimeter command ' '
  s' <- magic s splitCommand 
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

magic :: (String, FileSystem) -> [String] -> IO (String, FileSystem)
magic s [] = do
  return s
magic s ("pwd":args) = do
  pwd s
magic (wdir, fs) ["ls"] = do
  (x:currpath) <- splitByDelimeter wdir '/'
  ls (wdir, fs) [] (["/"] ++ currpath) (Just fs)
magic (wdir, fs) ["ls", arg] = do 
  (x:argpath) <- splitByDelimeter arg '/'
  if (x == "") then 
    ls (wdir, fs) ("/" ++ (intercalate "/" argpath)) (["/"] ++ argpath) (Just fs)
  else do
    (y:currpath) <- splitByDelimeter wdir '/'
    ls (wdir, fs) arg (["/"] ++ currpath ++ (x:argpath)) (Just fs)

pwd :: (Path, FileSystem) -> IO (Path, FileSystem)
pwd (str, fs) = putStr (str ++ "\n") >> return (str, fs)

ls :: (Path, FileSystem) -> String -> [String] -> Maybe FileSystem -> IO (Path, FileSystem)
ls s msg _ Nothing = putStr ("Cannot acces \'" ++ msg ++ "\': No such file or directory\n") >> return s
ls s msg [x] (Just (Regular file))
  | x == (fileName file) = putStr ((fileName file) ++ "\n") >> return s
  | otherwise = ls s msg [] Nothing
ls s msg [x] (Just (Directory dir))  
  | x == (dirName dir) = printAll (dirContent dir) >> return s
  | otherwise = ls s msg [] Nothing
  where printAll [] = putStr "\n"
        printAll ((Regular x):dir) = putStr (fileName x ++ "  ") >> printAll dir
        printAll ((Directory d):dir) = putStr ((dirName d) ++ "  ") >> printAll dir
ls s msg (x:xs) (Just (Directory dir)) 
  | x == (dirName dir) =  ls s msg xs (find (head xs) (dirContent dir))
  | otherwise = ls s msg [] Nothing
  where find str [] = Nothing
        find str ((Regular file):dirs) 
          | fileName file == str = Just (Regular file)
          | otherwise = find str dirs
        find str ((Directory dir):dirs) 
          | dirName dir == str = Just (Directory dir)
          | otherwise = find str dirs 

-- using these for testing
--
file = RegularFile "file1.txt" "Hello" 
file2 = RegularFile "file2.txt" "Bye!"
root = Directory (Dir "/" [Directory (Dir "home" [Regular file, Directory (Dir "etc" [Regular file2])]), Regular file2]) 
