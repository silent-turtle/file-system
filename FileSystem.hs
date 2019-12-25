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

-- using these for testing
--
file = File "file.txt" "Hello!"
file2 = File "file2.txt" "Bye!"

root = Directory "/" [(Directory "home/" [(Leaf file)]), (Leaf file2)] 

