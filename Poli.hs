{-# LANGUAGE TypeSynonymInstances, OverlappingInstances #-}

import Data.List
import Control.Monad.Instances
import Control.Monad
import Control.Arrow
import Data.Ord
import Data.Maybe
import Debug.Trace
import Data.Monoid
import Analitica


newtype Intervallo = Intervallo (Param,Param)

-- monoide per gli intervalli. Aggiungere un intervallo ad un'altro si intende considerare il secondo come variazione del primo
instance Monoid Intervallo where
	mempty = Intervallo (0,0)
	Intervallo (x1,x2) `mappend` Intervallo (x3,x4) = 
		Intervallo (x1,x2 + x4 - x3)

data Linea a = Linea Intervallo (Param -> a)
-- una linea parametrica è formata da un intervallo di esistenza per il suo parametro e una funzione che mappa il parametro nei punti

instance Functor Linea where
	f `fmap` (Linea i g) = Linea i $ f `fmap` g 

normale :: Intervallo
normale = Intervallo (0,1)

-- costruzione di una linea parametrica che rappresenrta un segmento
fromSeg :: Seg -> Linea Punto
fromSeg (p1,p2) = Linea normale $ \t -> p1 ^+ (t ^* (p2 ^- p1))

-- trasformazione dei una linea affinchè venga descritta su un intervallo diverso
remap :: Linea a -> Intervallo -> Linea a
remap (Linea (Intervallo (i0,i1)) f) j@(Intervallo (j0,j1)) = Linea j $ \t -> f (i0 + ((i1 - i0)/(j1 - j0)) * (t - j0))

-- versione particolare di remap che sposta l'intervallo senza scalarlo
reset :: Linea a -> Param -> Linea a 
reset l@(Linea (Intervallo (x1,x2)) _) x = remap l $ Intervallo (x1 + x,x2 + x) 

-- tentativo di estrarre un punto da una linea fornendo un parametro. Fallisce se il parametro è fuori dall'intervallo
punto :: Linea a -> Param -> Maybe a
punto (Linea (Intervallo (i0,i1)) f) p 
	| i0 < i1 && p >= i0 && p <= i1 = Just $ f p
	| i0 >= i1 && p <= i0 && p >= i1 = Just $ f p
	| True =  Nothing


-- monoide per le linee. Gli intervalli vengono composti con il loro monoide, le funzioni assemblate come sucessive. Se il secondo punto del primo segmento non coincide con il primo del secondo ci sarà una discontinuità al passaggio del parametro da un dominio all'altro.
instance Monoid a => Monoid (Linea a) where
	l1@(Linea i@(Intervallo (_,i1)) f) `mappend` l2@(Linea j _)  = 
		let pf = f i1 in Linea (i `mappend` j)  $ \t -> fromMaybe (fromJust (reset l2 i1 `punto` t)) $ l1 `punto` t
	mempty = Linea vuoto mempty

vuoto = Intervallo (0,0)


-- campiona una linea sul suo dominio, restituisce sia i parametri che i punti.
sample :: Int -> Linea a -> [(Param, a)]
sample n l@(Linea (Intervallo (i0,i1)) _ ) = catMaybes $ map (fmap . (,) &&& punto l >>> app) $ [i0, i0 + (i1 - i0)/fromIntegral n  .. i1]


-- prima associazione paramtro punto di una linea
base :: Linea a -> (Param,a)
base (Linea (Intervallo (i0,i1)) f) = (i0, f i0) 


ricercaMinimi :: (a -> Float) -> [a] -> [(a,a)]
ricercaMinimi f xs = do
	(l,x,r) <- zip3 xs (tail $ cycle xs) (tail  . tail $ cycle xs)
	guard (f l > f x && f r > f x) 
	return (l,r)

ricerca3 :: (a -> a -> a -> Bool) -> [a] -> [(a,(a,a))]
ricerca3 f xs = do
	(l,x,r) <- zip3 xs (tail $ cycle xs) (tail  . tail $ cycle xs)
	guard $ f l x r 
	return (x,(l,r))

ricercaLineare :: Int -> (a -> Float) -> Linea a -> (Param, Float)
ricercaLineare n f l = minimumBy (comparing snd) . map (second f) $ sample n l


change p1 p2 p3 = pendenza p1 p2 /= pendenza p2 p3

intersezione :: Int -> Linea Punto -> Linea Punto -> Maybe Param -> ((Linea Punto, Param), (Linea Punto,Param))
intersezione n f g mp = let 
	(s,d) = ricercaLineare n (distanza bg) f
	(t,bg) = fromMaybe (base g) $ do 
		p <- mp 
		(,) p `fmap` (g `punto` p) 
	in intersezione' n d f g (s,t)
	where	intersezione' n d f g (s, t)
			| d' == d = ((f,s),(g,t'))
			| otherwise = intersezione' n d' g f (t' , s) 		
	   		where (t',d') = ricercaLineare n (distanza . fromJust $ f `punto` s) g

  

modF y x = let
	n = floor (x/y)
	r = x - fromIntegral n * y 
	in if r < 0.5 then r else 1 - r

l1, l2 :: Linea Punto 
l1 = mconcat . map fromSeg . segmenti . map Punto $ [(0,0),(0,6),(3,8),(6,6),(6,0),(0,0)]
l2 = mconcat . map fromSeg . segmenti . map Punto $ [(0,2),(0,4),(1.5,4),(1.5,2),(0,2)]
