module Grafo (
    Vertice,
    Vertices,
    Aristas,
    Grafo,
    GrafoNoDirigido,
    GrafoTriangularNoDirigido,
    GrafoDSN,
    indexOf,
    grafoDSNdeLista,
    aristaDesdeDSN,
    grafoDdeLista,
    aristaDesde,
    grafoDeD,
    grafoNDdeLista,
    aristaEntreND,
    grafoDeND,
    grafoTNDdeLista,
    aristaEntreTND,
    grafoDeTND
) where

import Data.List
import Data.Maybe



type Vertice a = a
type Vertices a = [Vertice a]
type Aristas = [[Int]]
type Grafo a = (Vertices a, Aristas)

data GrafoDirigido a = GrafoD (Grafo a)
data GrafoNoDirigido a = GrafoND (Grafo a)
data GrafoTriangularNoDirigido a = GrafoTND (Grafo a)
data GrafoDSN = GrafoDSN Aristas --grafo dirigido sin nombres

indexOf :: Eq a => Vertice a -> Grafo a -> Maybe Int
indexOf v (vs, as) = elemIndex v vs

esFormaTriangular :: Aristas -> Bool
esFormaTriangular = snd . foldr (\filaAlta (filaBaja, tof) -> if length filaAlta == 1 + length filaBaja 
                                                      then (filaAlta, tof)
                                                      else (filaAlta, False)) ([], True)

esFormaRectangular :: Aristas -> Bool
esFormaRectangular as = all ((==) (length as) . length) as

standardTest = ((\vs as -> length vs == length as), "Número de vértices no es el mismo que el numero de filas en la matriz de aristas")

--GRAFO DIRIGIDO SIN NOMBRES
aristaDesdeDSN :: Vertice Int -> Vertice Int -> GrafoDSN -> Bool
aristaDesdeDSN v1 v2 (GrafoDSN as) =
    if v1 >= 0 && v2 >= 0 && v1 < length as && v2 < length as
    then (as !! v1) !! v2 == 1
    else False

grafoDSNdeLista :: Aristas -> GrafoDSN
grafoDSNdeLista as =  if esFormaRectangular as
                      then GrafoDSN as
                      else error "Aristas no son en forma rectangular"


--GRAFO DIRIGIDO
aristaDesde :: Eq a => Vertice a -> Vertice a -> GrafoDirigido a -> Bool
aristaDesde v1 v2 (GrafoD g) =
    let v1Index = fromMaybe (-1) $ indexOf v1 g
        v2Index = fromMaybe (-1) $ indexOf v2 g
        as = snd g
        in if v1Index > (-1) && v2Index > (-1)
        then (as !! v1Index) !! v2Index == 1
        else False

grafoDdeLista :: Vertices a -> Aristas -> GrafoDirigido a
grafoDdeLista vs as =
    if length vs /= length as
    then error "Número de vértices no es el mismo que el numero de filas en la matriz de aristas"
    else if esFormaRectangular as
         then GrafoD (vs, as)
         else error "Aristas no son en forma rectangular"

grafoDeD :: GrafoDirigido a -> Grafo a
grafoDeD (GrafoD g) = g
--GRAFO NO DIRIGIDO

aristaEntreND :: Eq a => Vertice a -> Vertice a -> GrafoNoDirigido a -> Bool
aristaEntreND v1 v2 (GrafoND g) =
    let v1Index = fromMaybe (-1) $ indexOf v1 g
        v2Index = fromMaybe (-1) $ indexOf v2 g
        as = snd g
        in if v1Index > (-1) && v2Index > (-1)
        then (as !! (min v1Index v2Index)) !! (max v1Index v2Index) == 1
        else False

grafoNDdeLista :: Vertices a -> Aristas -> GrafoNoDirigido a
grafoNDdeLista vs as =
    if length vs /= length as
    then error "Número de vértices no es el mismo que el numero de filas en la matriz de aristas"
    else if esFormaRectangular as
         then if as == transpose as
              then GrafoND (vs, as)
              else error "Matriz de aristas no es simétrica"
         else error "Matriz de aristas no son en forma rectangular"

grafoDeND :: GrafoNoDirigido a -> Grafo a
grafoDeND (GrafoND g) = g

--GRAFO TRIANGULAR Y NO DIRIGIDO
aristaEntreTND :: Eq a => Vertice a -> Vertice a -> GrafoTriangularNoDirigido a -> Bool
aristaEntreTND v1 v2 (GrafoTND g) =
    let v1Index = fromMaybe (-1) $ indexOf v1 g
        v2Index = fromMaybe (-1) $ indexOf v2 g
        as = snd g
        in if v1Index > (-1) && v2Index > (-1)
        then (as !! (min v1Index v2Index)) !! ((max v1Index v2Index) - (min v1Index v2Index)) == 1
        else False

grafoTNDdeLista :: Vertices a -> Aristas -> GrafoTriangularNoDirigido a
grafoTNDdeLista vs as =
    if length vs /= length as
    then error "Número de vértices no es el mismo que el numero de filas en la matriz de aristas"
    else if esFormaTriangular as
         then GrafoTND (vs, as)
         else error "Lista de aristas no son en forma triangular"


grafoTNDdeLista' :: Vertices a -> Aristas -> GrafoTriangularNoDirigido a
grafoTNDdeLista' vs' = GrafoTND . grafoPorPruebas (((\vs as -> esFormaTriangular as), "Lista de aristas no son en forma triangular"):[standardTest]) vs'

grafoDeTND :: GrafoTriangularNoDirigido a -> Grafo a
grafoDeTND (GrafoTND g) = g

grafoPorPruebas :: [((Vertices a -> Aristas -> Bool), String)] -> Vertices a -> Aristas  -> Grafo a
grafoPorPruebas ((test, errorStr):ts) vs as  = 
    if test vs as
    then if length ts == 0 
         then (vs, as) 
         else grafoPorPruebas ts vs as
    else error errorStr