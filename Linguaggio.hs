data Condizioni = IncrocioX | IncrocioY | Angolo deriving (Show,Eq)

newtype Linguaggio = Linguaggio (Condizioni -> Bool)
instance Show Linguaggio where
	show (Linguaggio f) = show (f IncrocioX,f IncrocioY, f Angolo)



c ?+ Linguaggio f = Linguaggio $ \x -> if c == x then True else f x
c ?- Linguaggio f = Linguaggio $ \x -> if c == x then False else f x

data Marchiato a = Marchiato Linguaggio a deriving Show

condiziona :: Marchiato a -> (Linguaggio -> Linguaggio) -> Marchiato a
condiziona (Marchiato l x) f = Marchiato (f l) x

instance Functor Marchiato where
	f `fmap` (Marchiato g x) = Marchiato g (f x)

