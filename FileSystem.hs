{-# LANGUAGE TypeApplications #-}

data File = File [String]
  deriving (Show, Read)

data Tree =
  Empty
  | Leaf File 
  | Directory String [Tree]
  deriving (Show, Read)


serialise :: Tree -> IO ()
serialise tree = writeFile "test.txt" (show tree)

deserialise :: IO Tree
deserialise = fmap (read @Tree) (readFile "test.txt")  


-- using these for testing
--
file = File ["file.txt", "Hello!"]
file2 = File ["file2.txt", "Bye!"]
root = Directory "/" [(Directory "home/" [(Leaf file), (Directory "etc/" [(Leaf file2)])]), (Leaf file2)] 
