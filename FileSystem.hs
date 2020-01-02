{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}


import Data.List

-- general comments:
-- you can use pure instead of return - it's the same, except you only need Applicative
--
-- you can use let bindings in a do block
-- instead of writing:
-- f = do
--   x' <- return (5 + 3)
--   ...
-- you can write
-- f = do
--   let x' = 5 + 3
--   ...
--
-- you already have a type synonym for Path
-- why did you ditch it one point and start using String - it's confusing
--
-- this is **exactly** why you should instead make a
-- newtype Path = Path {getPath :: String}
-- at the very least, so you can't "forget" to do this
-- and also so that I don't have to be confused what exactly a random String is

-- how about a newtype for names? so you can't accidentally put the file content as a name
-- e.g. you can't write this:
-- swappylol :: RegularFile -> RegularFile
-- swappylol (RegularFile x y) = RegularFile y x
--
-- add a Command type and parse it first, and then give it to magic, instead of doing parsing jit
--
-- you can return what you want to output as an element of a 3-tuple, instead of having IO everywhere,
-- just so you can print
--
-- naming is somewhat inconsistent eg:
-- the file type is RegularFile
-- but the dir type is Dir (not DirFile)

type Path = String
data RegularFile = RegularFile { fileName :: String, fileContent :: String } deriving (Show, Read)
-- ^ newlines for these? eg:
-- data RegularFile = RegularFile
--   { fileName :: String
--   , fileContent :: String
--   } deriving (Show, Read)
data Dir = Dir { dirName :: String, dirContent :: [FileSystem] } deriving (Show, Read)

data FileSystem
  = Regular RegularFile
  | Directory Dir
  deriving (Show, Read)

main :: IO ()
main = do
  f <- deserialise
  -- ^ this sould probably be named fs instead of f, since your final fs is fs'
  (dir, fs) <- createPair "/" f
  -- why bind "/" to a name to immediately use it below
  -- what you've written here is the exact same thing as only writing
  -- loop ("/", f)
  (_, fs') <- loop (dir, fs)
  serialise fs'

createPair :: Path -> FileSystem -> IO (Path, FileSystem)
-- ^ this function seems entirely pointless
-- you even call it only once
createPair dir fs = return (dir, fs)

loop :: (Path, FileSystem) -> IO (Path, FileSystem)
loop (path, fs) = do
  putStr (path ++ "$ ")
  command <- getLine
  let splitCommand = splitByDelimeter command ' '
  s' <- magic (path, fs) splitCommand
  loop s'

  -- s' <- magic (path, fs) splitCommand
  -- loop s'
  -- is the exact same thing as
  -- loop =<< magic (path, fs) splitCommand
  -- no need to make an arbitrary name (that also says nothing)
  -- (=<<) :: (a -> m b) -> m a -> m b
  -- (which looks awfully a lot like map, this is the type of concatMap if m was [])


serialise :: FileSystem -> IO ()
serialise fs = writeFile "test.txt" (show fs)
-- ^ easy eta reduce:
-- serialise = writeFile "test.txt" . show

deserialise :: IO FileSystem
deserialise = read <$> readFile "test.txt"

splitByDelimeter :: String -> Char -> [String]
splitByDelimeter str del = splitHelper str del []
  where splitHelper :: String -> Char -> [String] -> [String]
        splitHelper "" _ res = reverse res
        splitHelper s d res = splitHelper rest d (takeWhile (/= del) s : res)
          where rest' = dropWhile (/= del) s
                rest = if null rest' then [] else tail rest'
                       -- ^ no! bad! do a case match...
                       -- what's the point of doing if and then using tail in the else case
                       -- you could way more concisely do a case match and automatically get the case
                       --
                       -- also this is the same as drop 1
                       -- rest = drop 1 rest'

-- what does this name even mean? I literally have no idea what this function is supposed to do
filterPath :: [String] -> [String]
filterPath path = filterPathHelper path []
  where filterPathHelper :: [String] -> [String] -> [String]
        -- ^ write your binding on a new line to save indentation, e.g.:
        -- instead of
        --
        -- where filterPathHelper :: ...
        --
        -- write
        --
        -- where
        --   filterPathHelper :: ...
        filterPathHelper [] res = reverse res
        filterPathHelper [x] res = filterPathHelper [] (x:res)
        filterPathHelper ("" : ".." : xs) res = filterPathHelper xs ("":res)
        filterPathHelper (_ : ".." : xs) res = filterPathHelper xs res
        filterPathHelper (x:xs) res = filterPathHelper xs (x:res)

mix :: String -> String -> IO [String]
-- the IO here is redundant - in all cases you do *no* IO, and use return to
-- embed a pure value in IO
--
-- remove the IO from here!
--
-- also what does "mix" do or mean?
-- way too generic name
mix wdir arg = do
    -- ^ why wdir all of a sudden
  (x:argpath) <- return (splitByDelimeter arg '/')
  -- ^ this is the only place where you "implicitly" use IO
  -- (to throw an exception! great!)
  -- handle your case properly (i.e. return a Maybe or something)
  if x == "" then
    return (filterPath (x : argpath))
  else do
    let currpath = splitByDelimeter wdir '/'
    return (filterPath (currpath ++ (x : argpath)))
           -- ^ use ($) more


magic :: (String, FileSystem) -> [String] -> IO (String, FileSystem)
magic s [] = return s
magic s ("pwd":_) = pwd s
magic (wdir, fs) ["ls"] = do
  (_:currpath) <- return (splitByDelimeter wdir '/')
  -- ^ again, this could be a let binding instead
  -- and again, handle your empty case properly.
  -- or maybe return a NonEmpty from splitByDelimiter
  -- if you want it to always be so
  (res,fs') <- search fs [] ("/" : currpath) ls (Just fs)
  putStr res
  return (wdir, fs')
magic (wdir, fs) ["ls", arg] = do
  (_:filteredPath) <- mix wdir arg
  -- ^ this is actually a pure value, because mix is a pure value
  -- again, handle your empty case properly
  (res, fs') <- search fs arg ("/" : filteredPath) ls (Just fs)
  putStr res
  return (wdir, fs')
magic (_, fs) ["cd"] = return ("/", fs)
magic (wdir, fs) ["cd", arg] = do
  (x:filteredPath) <- mix wdir arg
  (wdir', fs') <-search fs (intercalate "/" (x:filteredPath) ++ "/")  ("/" : filteredPath) cd (Just fs)
  if null wdir' then
  -- what is the significance of an empty list here?
  -- what about returning a Maybe, instead of "remembering" that the empty list is a special case?
  -- (so that you can't forget)
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
  -- lol
  -- this entirely dissapears from here if you have proper command parsing
  putStrLn "rm: missing operand"
  return s
magic (wdir, fs) ["rm", arg] = do
  (x:filteredPath) <- mix wdir arg
  -- ^ mix is pure etc
  (res, fs') <- search fs (intercalate "/" (x:filteredPath)) ("/" : filteredPath) rm (Just fs)
  putStr res
  return (wdir, fs')
magic (wdir, fs) ("rm":arg:args) = do
  (x:filteredPath) <- mix wdir arg
  -- ^ mix is pure etc
  (res, fs') <- search fs (intercalate "/" (x:filteredPath)) ("/" : filteredPath) rm (Just fs)
  putStr res
  magic (wdir, fs') ("rm":args)
magic s (x:_) = do
  -- ^ same as the ["rm"] case
  putStrLn (x ++ ": command not found")
  return s

search :: FileSystem -> String -> [String] -> (FileSystem -> String -> FileSystem -> IO (String, FileSystem)) -> Maybe FileSystem -> IO (Path, FileSystem)
-- * use some newlines for this functions arguments, they are too many and hard to read:
-- search
--   :: FileSystem
--   -> String
--   -> [String]
--   -> (FileSystem -> String -> FileSystem -> IO (String, FileSystem))
--   -> Maybe FileSystem
--   -> IO (Path, FileSystem)
--
-- * I am 100% sure you are doing too many things at once, just by looking at the type of the function
--   it's way too complicated
--
-- I have literally no idea what any of the arguments except the first one mean - their types are too generic
-- for example:
-- * what is the callback for?
-- * what is the Maybe around FileSystem for?
--   it seems to me like you should handle this Maybe case in the spot where you call this function
--   and then call it with a FileSystem if there is none, instead of passing a Maybe down
search fs msg [] _ _ = putStr ("Cannot access \'" ++ msg ++ "\': No such file or directory\n") >> return ([], fs)
-- line is too long, better to put the other action on a newline:
-- search fs msg [] _ _
--   = putStr ("Cannot access \'" ++ msg ++ "\': No such file or directory\n")
--   >> return ([], fs)
--
-- also
-- x >> return y
--
-- is the same as
--
-- import Data.Functor (($>))
--
-- ($>) :: Functor f :: f a -> b -> f b
--
-- x $> y
--
-- (it ignores the result on the left and embeds the pure result on the right
-- you can remember it because it points to the result)
search fs msg _ _ Nothing = putStr ("Cannot access \'" ++ msg ++ "\': No such file or directory\n") >> return ([], fs)
-- ^ same comment as above, but also:
-- the string is the same, factor it out
search fs msg [x] f (Just (Regular file))
  | x == fileName file = f fs msg (Regular file)
  | otherwise = search fs msg [] f Nothing
search fs msg [x] f (Just (Directory dir))
  | x == dirName dir = f fs msg (Directory dir)
  | otherwise = search fs msg [] f Nothing
-- ^ these two cases are exactly the same, you could factor it out by making a
-- fileName :: FileSystem -> String
-- function
search fs msg (_:_:_) _ (Just (Regular _)) = putStr ("Cannot access \'" ++ msg ++ "\': No such file or directory\n") >> return ([], fs)
-- ^ same comment as above
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
          -- ^ again, more duplicate code that would disappear if you have a fileName


pwd :: (Path, FileSystem) -> IO (Path, FileSystem)
pwd (str, fs) = putStr (str ++ "\n") >> return (str, fs)
-- same comment as above
-- you can even eta-reduce this to:
-- pwd = (putStr (str ++ "\n") $>)

-- this function is pure, you're not doing any IO, remove the IO
ls :: FileSystem -> String -> FileSystem -> IO (String, FileSystem)
ls fs _ (Regular file) = return (fileName file ++ "\n", fs)
ls fs _ (Directory dir) = return (printAll (dirContent dir), fs)
-- ^ you could use records instead of the accessors (here and everywhere else):
--
-- {-# LANGUAGE NamedFieldPuns #-}
-- ls fs _ (Directory RegularFile{fileName}) = return (fileName ++ "\n", fs)
-- ls fs _ (Directory Dir{dirContent}) = return (printAll dirContent, fs)

  where printAll [] = "\n"
        -- would definitely not name this "print" as it's not actually doing IO
        -- also it's a good idea to write type signatures even in your where bindings
        printAll (Regular x : content) = fileName x ++ "  " ++ printAll content
        printAll (Directory d : content) = dirName d ++ "  " ++ printAll content


cd :: FileSystem -> String -> FileSystem -> IO (String, FileSystem)
cd fs newpath (Regular _) = putStr ("cd: \'" ++ newpath ++ "\': Not a directory\n") >> return ([], fs)
-- ^ same comment as above on
cd fs newpath (Directory _) = return (newpath, fs)

getContent :: FileSystem -> String -> FileSystem -> IO (String, FileSystem)
getContent fs msg (Directory _) = putStr ("cat: \'" ++ msg ++ "\': Is a directory\n") >> return ([], fs)
getContent fs _ (Regular file) = return (fileContent file, fs)

-- this function would also benefit from newtypes
-- because it wouldn't be String -> String -> ...
-- but it would be Contents -> Name -> ...
catToFile :: String -> String -> [FileSystem] -> [FileSystem]
catToFile content name [] = [Regular (RegularFile name content)]
-- ^ cat to an empty file definitely doesn't work like this
-- also if you make this return [] you could unite the duplicate code in the two other Regular cases below
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

-- another one of those functions with a thousand arguments
-- why do you need the function?
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
