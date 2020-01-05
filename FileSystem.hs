{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

import Data.List
import qualified Data.List.NonEmpty as N
import System.Directory

type Path = String
data RegularFile = RegularFile { fileName :: String, fileContent :: String } deriving (Show, Read)
data Dir = Dir { dirName :: String, dirContent :: [FileSystem] } deriving (Show, Read)

data FileSystem 
  = Regular RegularFile 
  | Directory Dir
  deriving (Show, Read)

data Command
  = Pwd
  | Ls [Path]
  | Cd (Maybe Path)
  | Cat [Path]
  | Rm (N.NonEmpty Path)
  | Exit

main :: IO ()
main = do
  f <- deserialise
  (dir, fs) <- createPair "/" f
  (_, fs') <- loop (dir, fs)
  serialise fs'


createPair :: Path -> FileSystem -> IO (Path, FileSystem)
createPair dir fs = return (dir, fs)


parseCommand :: [String] -> Either String Command
parseCommand ("pwd":_) = Right Pwd
parseCommand ("ls":path) = Right (Ls path)
parseCommand ["cd"] = Right (Cd Nothing)
parseCommand ["cd", path] = Right (Cd (Just path))
parseCommand ("cd":_) = Left "cd: too many arguments"
parseCommand ("cat":files) = Right (Cat files)
parseCommand ["rm"] = Left "rm: missing operand"
parseCommand ("rm": x : xs) = Right (Rm (x N.:| xs))
parseCommand ("exit":_) = Right Exit
parseCommand [] = Left ""
parseCommand _ = Left "invalid command"


loop :: (Path, FileSystem) -> IO (Path, FileSystem)
loop (workdir, fs) = do
  putStr (workdir ++ "$ ")
  input <- getLine
  let splitInput = splitByDelimeter input ' '
  case parseCommand splitInput of
    Left message -> do
      putStrLn message
      loop (workdir, fs)
    Right Exit -> do
      return (workdir, fs)
    Right command -> do
      s' <- executeCommand (workdir, fs) command
      loop s'


serialise :: FileSystem -> IO ()
serialise fs = writeFile "test.txt" (show fs)


deserialise :: IO FileSystem
deserialise = do
  fileExists <- doesFileExist "test.txt"
  if fileExists then read <$> readFile "test.txt"
  else return (Directory (Dir "/" []))


splitByDelimeter :: String -> Char -> [String]
splitByDelimeter str del = splitHelper str del []
  where splitHelper :: String -> Char -> [String] -> [String]
        splitHelper "" _ res = reverse res
        splitHelper s d res = splitHelper rest d (takeWhile (/= del) s : res)
          where rest' = dropWhile (/= del) s
                rest = if null rest' then [] else tail rest'

modifyPath :: [String] -> [String]
modifyPath path = modifyPathHelper path []
  where modifyPathHelper :: [String] -> [String] -> [String]
        modifyPathHelper [] res = reverse res
        modifyPathHelper [x] res = modifyPathHelper [] (x:res)
        modifyPathHelper ("" : ".." : xs) res = modifyPathHelper xs ("":res)
        modifyPathHelper (_ : ".." : xs) res = modifyPathHelper xs res
        modifyPathHelper (x:xs) res = modifyPathHelper xs (x:res)

splitAndModifyPath :: String -> String -> IO [String]
splitAndModifyPath wdir arg = do
  (x:argpath) <- return (splitByDelimeter arg '/')
  if x == "" then
    return (modifyPath (x : argpath))
  else do
    let currpath = splitByDelimeter wdir '/'
    return (modifyPath (currpath ++ (x : argpath)))
 

executeCommand :: (Path, FileSystem) -> Command -> IO (Path, FileSystem)
executeCommand s Exit = return s

executeCommand s Pwd = pwd s

executeCommand (workdir, fs) (Ls []) = do
  (_:currpath) <- return (splitByDelimeter workdir '/')
  (res,fs') <- search fs [] ("/" : currpath) ls (Just fs)
  putStr res
  return (workdir, fs')
executeCommand (workdir, fs) (Ls [arg]) = do
  (_:modifiedPath) <- splitAndModifyPath workdir arg
  (res, fs') <- search fs arg ("/" : modifiedPath) ls (Just fs)
  putStr res
  return (workdir, fs')
executeCommand (workdir, fs) (Ls (arg:args)) = do
  go (arg:args)
  where go [] = return (workdir, fs)
        go (x:xs) = do
          (_:modifiedPath) <- splitAndModifyPath workdir x
          (res, _) <- search fs x ("/" : modifiedPath) ls (Just fs)
          putStr (x ++ ":\n" ++ res)
          go xs

executeCommand (_, fs) (Cd Nothing) = return ("/", fs)
executeCommand (workdir, fs) (Cd (Just arg)) = do
  (x:modifiedPath) <- splitAndModifyPath workdir arg
  (workdir', fs') <- search fs (intercalate "/" (x:modifiedPath) ++ "/")  ("/" : modifiedPath) cd (Just fs)
  if null workdir' then
    return (workdir, fs')
  else
    return (workdir', fs')

executeCommand s (Cat []) = do
  x <- getLine
  go x
  where go "." = return s
        go str = do
          putStrLn str
          str' <- getLine
          go str'
executeCommand (workdir, fs) (Cat [">", file]) = do
  x <- getLine
  content <- go x []
  (_:modifiedPath) <- splitAndModifyPath workdir file
  return (workdir, modifyFileSystem fs content modifiedPath catToFile)
  where go :: String -> String -> IO String
        go "." result = return result
        go str [] = do
          str' <- getLine
          go str' str
        go str result = do
          str' <- getLine
          go str' (result ++ "\n" ++ str)
executeCommand (workdir, fs) (Cat (arg:args)) = do
  go [] (arg:args)
  where go :: String -> [String] -> IO (String, FileSystem) 
        go content [] = putStrLn content >> return (workdir, fs)
        go content [file] = do
          (x:modifiedPath) <- splitAndModifyPath workdir file
          (res,_) <- search fs (intercalate "/" (x : modifiedPath) ++ "/") ("/" : modifiedPath) getContent (Just fs)
          go (content ++ res) []
        go content [file, ">", outputfile] = do
          (x:modifiedPath) <- splitAndModifyPath workdir file
          (res, _) <- search fs (intercalate "/" (x : modifiedPath) ++ "/") ("/" : modifiedPath) getContent (Just fs)
          go (content ++ res) [">", outputfile]
        go content [">", file] = do
          (_:modifiedPath) <- splitAndModifyPath workdir file
          return (workdir, modifyFileSystem fs content modifiedPath catToFile)
        go _ (">":_) = do
          putStrLn "error: should have only one output file"
          return (workdir, fs)
        go content (y:ys) = do
          (x:modifiedPath) <- splitAndModifyPath workdir y
          (res, _) <- search fs (intercalate "/" (x : modifiedPath) ++ "/") ("/" : modifiedPath) getContent (Just fs)
          go (content ++ res ++ "\n") ys 

executeCommand (workdir, fs) (Rm (arg N.:| args)) = do
  go (workdir, fs) (arg : args)
  where go s [] = return s
        go (wd, f) (y : ys) = do
          (x:modifiedPath) <- splitAndModifyPath wd y
          (res, f') <- search f (intercalate "/" (x : modifiedPath)) ("/" : modifiedPath) rm (Just f)
          putStr res
          go (wd,f') ys


search :: FileSystem -> Path -> [Path] -> (FileSystem -> Path -> FileSystem -> IO (String, FileSystem)) -> Maybe FileSystem -> IO (String, FileSystem)
search fs path [] _ _ = putStr ("Cannot access \'" ++ path ++ "\': No such file or directory\n") >> return ([], fs)
search fs path _ _ Nothing = putStr ("Cannot access \'" ++ path ++ "\': No such file or directory\n") >> return ([], fs)
search fs path [x] f (Just (Regular file))
  | x == fileName file = f fs path (Regular file)
  | otherwise = search fs path [] f Nothing
search fs path [x] f (Just (Directory dir))
  | x == dirName dir = f fs path (Directory dir)
  | otherwise = search fs path [] f Nothing
search fs path (_:_:_) _ (Just (Regular _)) = putStr ("Cannot access \'" ++ path ++ "\': No such file or directory\n") >> return ([], fs)
search fs path (x:y:xs) f (Just (Directory dir))
  | x == dirName dir = search fs path (y:xs) f (find' y (dirContent dir))
  | otherwise = search fs path [] f Nothing
  where find' :: Path -> [FileSystem] -> Maybe FileSystem 
        find' _ [] = Nothing
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

cd :: FileSystem -> Path -> FileSystem -> IO (String, FileSystem)
cd fs newpath (Regular _) = putStr ("cd: \'" ++ newpath ++ "\': Not a directory\n") >> return ([], fs)
cd fs newpath (Directory _) = return (newpath, fs)

getContent :: FileSystem -> Path -> FileSystem -> IO (String, FileSystem)
getContent fs path (Directory _) = putStr ("cat: \'" ++ path ++ "\': Is a directory\n") >> return ([], fs)
getContent fs _ (Regular file) = return (fileContent file, fs)

rm :: FileSystem -> Path -> FileSystem -> IO (String, FileSystem)
rm fs path (Directory _) = putStr ("rm: cannot remove \'" ++ path ++ "\': is directory\n") >> return ([], fs)
rm fs path (Regular _) = return ([], fs')
  where (_:splitPath) = splitByDelimeter path '/'
        fs' = modifyFileSystem fs [] splitPath deleteFile
        deleteFile :: String -> String -> [FileSystem] -> [FileSystem]
        deleteFile _ _ [] = []
        deleteFile x name (Regular f : cont)
          | name == fileName f = deleteFile x name cont
          | otherwise = Regular f : deleteFile x name cont
        deleteFile x name (Directory d : cont) = Directory d : deleteFile x name cont


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

modifyFileSystem :: FileSystem -> String -> [String] -> (String -> String -> [FileSystem] -> [FileSystem]) -> FileSystem
modifyFileSystem (Regular f) _ _ _ = Regular f
modifyFileSystem (Directory (Dir name [])) _ _ _ = Directory (Dir name [])
modifyFileSystem (Directory d) _ [] _ = Directory d
modifyFileSystem (Directory d) content [filename] g = Directory (Dir (dirName d) (g content filename (dirContent d)))
modifyFileSystem (Directory (Dir name dircont)) content (x:xs) g = Directory (Dir name (copyDirContent dircont (x:xs)))
  where copyDirContent :: [FileSystem] -> [String] -> [FileSystem]
        copyDirContent [] _ = []
        copyDirContent [Regular f] _ = [Regular f]
        copyDirContent [Directory d] [] = [Directory d]
        copyDirContent (el:rest) [] = el : copyDirContent rest []
        copyDirContent [Directory d] (y:ys)
          | dirName d == y = [modifyFileSystem (Directory d) content ys g]
          | otherwise = [modifyFileSystem (Directory d) content (y:ys) g]
        copyDirContent (Regular f : cont) (y:ys) = modifyFileSystem (Regular f) content (y:ys) g : copyDirContent cont (y:ys)
        copyDirContent (Directory d : cont) (y:ys)
          | dirName d == y = modifyFileSystem (Directory d) content ys g : copyDirContent cont (y:ys)
          | otherwise = modifyFileSystem (Directory d) content (y:ys) g : copyDirContent cont (y:ys)
