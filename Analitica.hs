{-# LANGUAGE TypeSynonymInstances, OverlappingInstances #-}
module Analitica where

import Data.Monoid 
import Control.Monad 
import Control.Monad.Instances 

newtype Punto = Punto (Float,Float) deriving Show

xpunto (Punto (x,y)) = x
ypunto (Punto (x,y)) = y

distanza :: Punto -> Punto -> Float
distanza (Punto (x1,y1)) (Punto (x2,y2)) = sqrt $ (x2 - x1) ^ 2  + (y2 - y1) ^ 2

instance Monoid Punto where
	mappend = (^+)
	mempty = Punto (0,0)

-- un segmento descritto come una coppia di punti
type Seg = (Punto,Punto)

segmenti :: [Punto] -> [Seg]
segmenti = zip `ap` tail


pendenza :: Punto -> Punto -> Float
pendenza (Punto (x1,y1)) (Punto (x2,y2)) = (y2 - y1) * (x2 - x1)
{-
data Ordine = Orario | Antiorario | Allineati deriving Eq
ord :: Punto -> Punto -> Punto -> Ordine
ord p q r 
	| d < 0 = Antiorario
	| d > 0 = Orario
	| otherwise = Allineati
	where d = det (p ^- q) (r ^- q)

det :: Punto -> Punto -> Float
det (u1,u2) (v1,v2) = u1 * v2 - u2 * v1


incrocia :: Seg -> Seg -> Bool
incrocia (p1,p2) (q1,q2) = ord p1 p2 q1 /= ord p1 p2 q2

incrociano s1 s2 = incrocia s1 s2 && incrocia s2 s1

-}
type Param = Float

-- somma di 2 punti
(^+) :: Punto -> Punto -> Punto
(Punto (x1,x2)) ^+ (Punto (y1,y2)) = Punto (x1 + y1, x2 + y2)


-- moltiplicazione per una costante
(^*) :: Param -> Punto -> Punto
t ^* (Punto (x1,x2))  = Punto (t * x1,t * x2)

(^-) :: Punto -> Punto -> Punto
p1 ^- p2 = p1 ^+ ((-1) ^* p2)

