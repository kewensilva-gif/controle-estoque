module Utils where

splitBy :: Char -> String -> [String]
splitBy _ [] = [""]
splitBy delimiter (c:cs)
    | c == delimiter = "" : rest
    | otherwise = (c : head rest) : tail rest
    where
        rest = splitBy delimiter cs

isPrefixOf2 :: String -> [String] -> Bool
isPrefixOf2 _ [] = False
isPrefixOf2 comparador (l:ls) 
    | comparador == l = True
    | otherwise = isPrefixOf2 comparador ls

insereAtbs :: String -> String -> IO()
insereAtbs = writeFile

map2 :: (a -> b) -> [a] -> [b]
map2 _ [] = []
map2 f (l:ls) = f l:map2 f ls 

filter2 :: (a -> Bool) -> [a] -> [a]
filter2 _ [] = []
filter2 f (l:ls) 
    | f l = l:filter2 f ls
    | otherwise = filter2 f ls

filterMap :: (a -> Bool) -> (a -> b) -> [a] -> [b]
filterMap _ _ [] = []
filterMap f g (l:ls) 
    | f l = g l:filterMap f g ls
    | otherwise = filterMap f g ls

