module Grafos (
    Vertice,
    Vertices,
    Aristas,
    Grafo,
    GrafoTipo,
    indiceDe,
    grafoDe,
    grafoDdeLista,
    grafoDSNdeLista,
    grafoNDdeLista,
    grafoTNDdeLista,
    aristaEntre
) where

import Data.List
import Data.Maybe

type Vertice a = a
type Vertices a = [Vertice a]
type Aristas = [[Int]]
type Grafo a = (Vertices a, Aristas)

indiceDe :: Eq a => Vertice a -> Grafo a -> Maybe Int
indiceDe v (vs, as) = elemIndex v vs

{-TIPOS DE GRAFOS-}

data GrafoTipo a = GrafoD a | GrafoDSN a | GrafoND a | GrafoTND a --GrafoDSN es una excepción de la regla

instance Functor GrafoTipo where
  fmap f (GrafoD g) = GrafoD (f g)
  fmap f (GrafoDSN g) = GrafoDSN (f g)
  fmap f (GrafoND g) = GrafoND (f g)
  fmap f (GrafoTND g) = GrafoTND (f g)

grafoDe :: GrafoTipo (Grafo a) -> Grafo a
grafoDe (GrafoD g) = g
grafoDe (GrafoDSN g) = g
grafoDe (GrafoND g) = g
grafoDe (GrafoTND g) = g

aristaEntre :: Eq a => Vertice a -> Vertice a -> GrafoTipo (Grafo a) -> Maybe Int --quizás hay un nombre mejor para grafos dirigidos, y quizás hay abstracciones posibles
aristaEntre v1 v2 (GrafoD g) =
    let v1Indice = fromMaybe (-1) $ indiceDe v1 g
        v2Indice = fromMaybe (-1) $ indiceDe v2 g
        as = snd g
        in if v1Indice > (-1) && v2Indice > (-1)
           then Just ((as !! v1Indice) !! v2Indice)
           else Nothing
aristaEntre v1 v2 (GrafoDSN g) =
    let v1Indice = fromMaybe (-1) $ indiceDe v1 g
        v2Indice = fromMaybe (-1) $ indiceDe v2 g
        as = snd g
        in if v1Indice > (-1) && v2Indice > (-1)
           then Just ((as !! v1Indice) !! v2Indice)
           else Nothing
aristaEntre v1 v2 (GrafoND g) =
    let v1Indice = fromMaybe (-1) $ indiceDe v1 g
        v2Indice = fromMaybe (-1) $ indiceDe v2 g
        as = snd g
        in if v1Indice > (-1) && v2Indice > (-1)
           then Just ((as !! (min v1Indice v2Indice)) !! (max v1Indice v2Indice))
           else Nothing
aristaEntre v1 v2 (GrafoTND g) =
    let v1Indice = fromMaybe (-1) $ indiceDe v1 g
        v2Indice = fromMaybe (-1) $ indiceDe v2 g
        as = snd g
        in if v1Indice > (-1) && v2Indice > (-1)
           then Just ((as !! (min v1Indice v2Indice)) !! ((max v1Indice v2Indice) - (min v1Indice v2Indice)))
           else Nothing

{-Constructores para tipos de grafos-}

grafoDdeLista :: Vertices a -> Aristas -> GrafoTipo (Grafo a)
grafoDdeLista vs as = GrafoD $ grafoTipoPorPruebas [numeroPrueba, cuadradoPrueba] vs as

grafoDSNdeLista :: Aristas -> GrafoTipo (Grafo Int)
grafoDSNdeLista as =  GrafoDSN $ grafoTipoPorPruebas [cuadradoPrueba] [1..length as] as

grafoNDdeLista :: Vertices a -> Aristas -> GrafoTipo (Grafo a)
grafoNDdeLista vs as = GrafoND $ grafoTipoPorPruebas [numeroPrueba, cuadradoPrueba, simetricaPrueba] vs as -- cuadradoPrueba es más probable redudante considerando simetricaPrueba

grafoTNDdeLista :: Vertices a -> Aristas -> GrafoTipo (Grafo a)
grafoTNDdeLista vs as = GrafoTND $ grafoTipoPorPruebas [numeroPrueba, triangularPrueba] vs as


{-PRUEBAS-}

grafoTipoPorPruebas :: [((Vertices a -> Aristas -> Bool), String)] -> Vertices a -> Aristas  -> Grafo a
grafoTipoPorPruebas ((test, errorStr):ts) vs as  = 
    if test vs as
    then if length ts == 0 
         then (vs, as) 
         else grafoTipoPorPruebas ts vs as
    else error errorStr

numeroPrueba = ((\vs as -> length vs == length as), "Número de vértices no es el mismo que el numero de filas en la matriz de aristas")
cuadradoPrueba = ((\_ as -> esFormaCuadrado as), "Lista de aristas no son en forma rectangular")
triangularPrueba = ((\_ as -> esFormaTriangular as), "Lista de aristas no son en forma triangular")
simetricaPrueba = ((\_ as -> as == transpose as), "Matriz de aristas no es simétrica")

esFormaTriangular :: Aristas -> Bool
esFormaTriangular = snd . foldr (\filaAlta (filaBaja, tof) -> if length filaAlta == 1 + length filaBaja 
                                                      then (filaAlta, tof)
                                                      else (filaAlta, False)) ([], True)
esFormaCuadrado :: Aristas -> Bool
esFormaCuadrado as = all ((==) (length as) . length) as