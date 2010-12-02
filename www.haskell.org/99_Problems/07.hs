
data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]

flatten (Elem a) = [a]
flatten (List []) = []
flatten (List (x:xs)) = (flatten x) ++ (flatten (List xs))
