{-# LANGUAGE TypeSynonymInstances, OverlappingInstances #-}

import Data.List
import Control.Monad.Instances
import Control.Monad
import Control.Arrow
import Data.Ord
import Data.Maybe
import Data.Ratio
import Debug.Trace
import Data.Monoid
import Analitica



data Linea a = Linea Param (Param -> a)
-- una linea parametrica è formata da un intervallo di esistenza per il suo parametro e una funzione che mappa il parametro nei punti

instance Functor Linea where
	f `fmap` (Linea p g) = Linea p $ f `fmap` g 

-- costruzione di una linea parametrica che rappresenrta un segmento
fromSeg :: Seg -> Linea Punto
fromSeg (p1,p2) = Linea (toRational $ distanza p1 p2) $ \t ->  p1 ^+ (t ^* (p2 ^- p1))


modR x y = let
	z = x / y
	dz = denominator z 
	in (numerator z `mod` dz) % dz

	
-- tentativo di estrarre un punto da una linea fornendo un parametro. Fallisce se il parametro è fuori dall'intervallo
punto :: Linea a -> Param -> a
punto (Linea q f) p = f p


-- monoide per le linee. Gli intervalli vengono composti con il loro monoide, le funzioni assemblate come sucessive. Se il secondo punto del primo segmento non coincide con il primo del secondo ci sarà una discontinuità al passaggio del parametro da un dominio all'altro.
instance Monoid a => Monoid (Linea a) where
	l1@(Linea p1 f) `mappend` l2@(Linea p2 g)  = 
		Linea p' $ \t -> let
			t' = t * p' 
			in if t' < p1 then f (t'/p1) else g $ (t' - p1)/p2
		where p' = p1 + p2
	mempty = Linea 0 mempty

-- campiona una linea sul suo dominio, restituisce sia i parametri che i punti.
sample :: Int -> Linea a -> [(Param, a)]
sample n (Linea _ f) = map (id &&& f) $ [0, 1/fromIntegral n  .. 1]

esplosione :: Int -> Linea a -> Linea (Linea a)
esplosione n (Linea p f) = Linea p $ \t -> Linea (dp  * p) $ \ti -> f $ t + ti*dp where
	dp = 1/fromIntegral n

dico :: (a -> Float) -> 

{-
ricercaMinimi :: (a -> Float)\x -> if c == x then True else f x
 -> [a] -> [(a,a)]
ricercaMinimi f xs = do
	(l,x,r) <- zip3 xs (tail $ cycle xs) (tail  . tail $ cycle xs)
	guard (f l > f x && f r > f x) 
	return (l,r)

espandi3 xs = zip3 xs (tail $ cycle xs) (tail  . tail $ cycle xs)

 
ricerca3 :: (a -> a -> a -> Bool) -> [a] -> [(a,(a,a))]
ricerca3 f xs = do
	(l,x,r) <- zip3 xs (tail $ cycle xs) (tail  . tail $ cycle xs)
	guard $ f l x r 
	return (x,(l,r))

ricercaLineare :: Int -> (a -> Float) -> Linea a -> (Param, Float)
ricercaLineare n f l = minimumBy (comparing snd) . map (second f) $ sample n l


change p1 p2 p3 = p2 ^- p1 /= p3 ^- p2

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

  




-}
l1, l2 :: Linea Punto 
l1 = mconcat . map fromSeg . segmenti . map Punto $ [(0,0),(0,6000),(3000,8000),(6000,6000),(6000,0),(0,0)]
l2 = mconcat . map fromSeg . segmenti . map Punto $ [(0,0),(0,2450),(2040,2450),(2040,0),(0,0)]
