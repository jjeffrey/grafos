{-# LANGUAGE FlexibleInstances #-}
module Grafos (
    Vertice,
    Vertices,
    Aristas,
    Grafo,
    ValGrafo,
    GrafoTipo (GrafoD, GrafoDSN, GrafoND, GrafoTND),
    indiceDe,
    grafoDe,
    valGrafodeLista,
    grafoDSNdeLista,
    aristaEntre
) where

import Data.List
import Data.Maybe
import Data.Function
import Data.Tuple.Curry

type Vertice a = a
type Vertices a = [Vertice a]
type Aristas = [[Int]]
type Grafo a = (Vertices a, Aristas)

indiceDe :: Eq a => Vertice a -> Grafo a -> Maybe Int
indiceDe v (vs, as) = elemIndex v vs

showGrafo :: Show a => Grafo a -> String
showGrafo g = "Not yet implemented/Normal show: " ++ show g


{- VALGRAFOS -}

data ValGrafo a = ValGrafo GrafoTipo a
data GrafoTipo = GrafoD
               | GrafoDSN
               | GrafoND
               | GrafoTND

instance Functor ValGrafo where
    fmap f (ValGrafo gt g) = ValGrafo gt (f g)

instance Show a => Show (ValGrafo (Grafo a)) where
    show (ValGrafo _ grafo) = showGrafo grafo

grafoDe :: ValGrafo (Grafo a) -> Grafo a
grafoDe (ValGrafo _ g) = g

{-Constructores de ValGrafos-}
valGrafodeLista :: GrafoTipo -> Vertices a -> Aristas -> ValGrafo (Grafo a)
valGrafodeLista gt vs as = ValGrafo gt $ uncurryN grafoTipoPorPruebas $
    case gt of
        GrafoD   -> ([numeroPrueba, cuadradoPrueba], vs, as)
        GrafoND  -> ([numeroPrueba, cuadradoPrueba, simetricaPrueba], vs, as)
        GrafoTND -> ([numeroPrueba, triangularPrueba], vs, as)
        _        -> error "GrafoTipo invalído: usa el constructor especial."

grafoDSNdeLista :: Aristas -> ValGrafo (Grafo Int)
grafoDSNdeLista as =
    ValGrafo GrafoDSN $ grafoTipoPorPruebas [cuadradoPrueba] [1..length as] as

{-Funciones para ValGrafos-}
aristaEntre :: Eq a => Vertice a -> Vertice a -> ValGrafo (Grafo a) -> Maybe Int
aristaEntre v1 v2 (ValGrafo gt g@(_,as)) =
    let i1 = fromMaybe (-1) $ indiceDe v1 g
        i2 = fromMaybe (-1) $ indiceDe v2 g
        in if i1 > (-1) && i2 > (-1)
          then Just $ case gt of
              GrafoND    -> (as !! min i1 i2) !! max i1 i2
              GrafoTND   -> (as !! min i1 i2) !! (max i1 i2 - min i1 i2)
              otherwise  -> (as !! i1) !! i2
          else Nothing


{- PRUEBAS -}

grafoTipoPorPruebas :: [(Vertices a -> Aristas -> Bool, String)] -> Vertices a -> Aristas -> Grafo a
grafoTipoPorPruebas ((test, errorStr):ts) vs as  = 
    if test vs as
    then if null ts
        then (vs, as)
        else grafoTipoPorPruebas ts vs as
    else error errorStr

numeroPrueba =
    (\vs as -> length vs == length as,
     "Número de vértices no es iqual al número de filas en la matriz de aristas")
cuadradoPrueba =
    (\_  as -> esFormaCuadrado as,
     "Lista de aristas no son en forma rectangular")
triangularPrueba =
    (\_  as -> esFormaTriangular as,
     "Lista de aristas no son en forma triangular")
simetricaPrueba  =
    (\_  as -> as == transpose as,
     "Matriz de aristas no es simétrica")

esFormaTriangular :: Aristas -> Bool
esFormaTriangular = snd . foldr (\filaAlta (filaBaja, tof) ->
                        if length filaAlta == 1 + length filaBaja
                        then (filaAlta, tof)
                        else (filaAlta, False)) ([], True)

esFormaCuadrado :: Aristas -> Bool
esFormaCuadrado as = all ((==) (length as) . length) as
