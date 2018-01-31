isJust :: Maybe a -> Bool
isJust (Just a) = True
isJust Nothing  = False

isNothing :: Maybe a -> Bool
isNothing (Just a) = False
isNothing Nothing  = True

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee _ f (Just x)  = f x
mayybee def _ Nothing = def

fromMaybe :: a -> Maybe a -> a
fromMaybe _ (Just x) = x
fromMaybe d Nothing  = d

listToMaybe :: [a] -> Maybe a
listToMaybe []    = Nothing
listToMaybe (x:_) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing  = []
maybeToList (Just x) = [x]

catMaybes :: [Maybe a] -> [a]
catMaybes list = foldr foldFunc [] list
  where foldFunc  = (\a b -> case a of
                            Nothing -> b
                            Just x  -> x : b
                 )

