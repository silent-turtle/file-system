{-# LANGUAGE TypeApplications #-}

data File = File [String]
  deriving (Show, Read)

data Tree =
  Empty
  | Leaf File 
  | Directory String [Tree]
  deriving (Show, Read)

instance Show Tree where
  show Empty = "{}" 
  show (Leaf x) = filename x
  show (Directory root []) = "{ " ++ root ++ " : {} } "
  show (Directory root children) = "{ " ++ root ++ " : " ++ showChildren children ++ " }"
    where showChildren [] = ""
          showChildren [x] = show x
          showChildren (x:xs) = show x ++ ", " ++ showChildren xs

serialise :: Tree -> IO ()
serialise tree = writeFile "test.txt" (show tree)

deserialise :: IO Tree
deserialise = fmap (read @Tree) (readFile "test.txt")  

root = Directory "/" [(Directory "home/" [(Leaf file)]), (Leaf file2)] 

