type Dictionary tKey tValue = [(tKey, tValue)]
deriving (Show)

myIsNull :: Dictionary tKey tValue -> Bool
myIsNull [] = True
myIsNull _ = False

mySize :: Dictionary tKey tValue -> Int
mySize = foldl (\n _ -> succ n) 0

myFind :: Eq tKey => tKey -> Dictionary tKey tValue -> Maybe tValue
myFind _ [] = Nothing
myFind k ((key,value):tail) = 
	if k == key
	then Just value
	else myFind k tail
	
myInsert :: (Eq tKey) => tKey -> tValue -> Dictionary tKey tValue -> Dictionary tKey tValue 
myInsert tKey tValue [] = (tKey, tValue):[]
myInsert tKey tValue ((key,value):tail) =
	if tKey == key
	then (key,value):tail
	else (key, value):(myInsert tKey tValue tail)
	
myUpdate :: (Eq tKey) => tKey -> tValue -> Dictionary tKey tValue -> Dictionary tKey tValue
myUpdate tKey tValue [] = (tKey, tValue):[]
myUpdate tKey tValue ((key,value):tail) =
	if tKey == key
	then (key, tValue):tail
	else (key, value):(myUpdate tKey tValue tail)

myDelete :: (Eq tKey) => tKey -> Dictionary tKey tValue -> Dictionary tKey tValue
myDelete _ [] = []
myDelete tKey ((key,value):tail) =
	if tKey == key
	then myDelete tKey tail
	else (key, value):(myDelete tKey tail)
