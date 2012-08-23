find_last :: [a] -> a
find_last (h:[]) = h
find_last (h:t) = find_last t

find_next_to_last :: [a] -> a
find_next_to_last (h:[x]) = h
find_next_to_last (h:t) = find_next_to_last t

find_nth :: [a] -> Int -> a
find_nth (h:t) 1 = h 
find_nth (h:t) position = if position < 1
                            then
                              error "index out of bounds"
                            else
                              find_nth t (position - 1) 

number_of_elements :: [a] -> Int
number_of_elements [] = 0
number_of_elements (h:t) = 1 + (number_of_elements t)

reverse_list :: [a] -> [a]
reverse_list [] = []
reverse_list (h:t) = (reverse_list t) ++ [h]

is_palindrome :: (Eq a) => [a] -> Bool
is_palindrome [] = True
is_palindrome [_] = True
is_palindrome (h:t) = (h == (last t)) && ( is_palindrome (init t) )


data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem a) = [a]
flatten (List (x:xs)) = (flatten x ) ++ (flatten (List xs))
flatten (List []) = []

compress :: Eq a => [a] -> [a]
compress xs = foldl (\ac val -> if val == (last ac) then ac else ac ++ [val]) [head xs] (tail xs)

pack :: Eq a => [a] -> [[a]]
pack [] = []
pack (h:t) = (h:(takeWhile (== h) t)):pack(dropWhile (==h) t)

encode :: Eq a => [a] -> [(Int, a)]
encode list = map (\val -> (length val , head val)) (pack list)
