-- =====================================================================
--  CI-3661 — Tarea 1 (Haskell) — Septiembre–Diciembre 2026
--
--  Nombre: Gabriel Jose Rojas Teran
--  Carnet: 15-11278
--
--  Declaración de uso de herramientas de IA (obligatoria, ver enunciado):
--
-- =====================================================================
--  No cambies el nombre del módulo ni la lista de exportación, ni las
--  firmas de las funciones. Puedes agregar todas las funciones auxiliares
--  que quieras. El único import permitido es Data.Char.
-- =====================================================================
module Tarea1
  ( esPalindromo
  , mayorYMenor
  , leerHora
  , duracion
  , promedioSobreUmbral
  , movimientosCaballo
  , enDosSaltos
  , miMap
  , miFilter
  , miFoldl
  ) where

import Data.Char (isAlphaNum, isDigit, toLower, digitToInt)

-- =====================================================================
-- Parte A: recursión explícita.
-- Prohibido: map, filter, foldr, foldl, listas por comprensión, reverse,
-- maximum, minimum, all, any y cualquier otra función de orden superior.
-- =====================================================================

-- A1
esPalindromo :: String -> Bool
esPalindromo s = limpia == invertir limpia
  where
    limpia = limpiar s

-- Deja solo letras y digitos, en minuscula.
limpiar :: String -> String
limpiar [] = []
limpiar (c:cs)
  | isAlphaNum c = toLower c : limpiar cs
  | otherwise    = limpiar cs

invertir :: [a] -> [a]
invertir xs = invertirAux xs []

invertirAux :: [a] -> [a] -> [a]
invertirAux [] acc     = acc
invertirAux (x:xs) acc = invertirAux xs (x : acc)

-- A2
-- Explica aquí, en dos o tres líneas, por qué tu solución recorre la
-- lista una sola vez:
-- La función procesa la lista mediante una función auxiliar recursiva con
-- acumuladores, comparando y actualizando el máximo y mínimo simultáneamente
-- en cada elemento. De esta forma, realiza un único recorrido lineal (O(n)).
mayorYMenor :: Ord a => [a] -> Maybe (a, a)
mayorYMenor []     = Nothing
mayorYMenor (x:xs) = Just (aux xs x x)
  where
    aux [] maxVal minVal     = (maxVal, minVal)
    aux (y:ys) maxVal minVal =
      let maxVal' = if y > maxVal then y else maxVal
          minVal' = if y < minVal then y else minVal
      in aux ys maxVal' minVal'

-- A3
leerHora :: String -> Either String (Int, Int)
leerHora [a, b, ':', c, d]
  | isDigit a && isDigit b && isDigit c && isDigit d =
      let h = digitToInt a * 10 + digitToInt b
          m = digitToInt c * 10 + digitToInt d
      in if h > 23
         then Left "hora fuera de rango"
         else if m > 59
              then Left "minutos fuera de rango"
              else Right (h, m)
leerHora _ = Left "formato inválido"

duracion :: String -> String -> Either String Int
duracion s1 s2 = case leerHora s1 of
  Left err1 -> Left err1
  Right (h1, m1) -> case leerHora s2 of
    Left err2 -> Left err2
    Right (h2, m2) ->
      let t1 = h1 * 60 + m1
          t2 = h2 * 60 + m2
          diff = t2 - t1
      in if diff < 0
         then Right (diff + 1440)
         else Right diff

-- =====================================================================
-- Parte B: orden superior.
-- Prohibido: recursión explícita (ninguna función de esta parte, ni sus
-- auxiliares, puede llamarse a sí misma).
-- =====================================================================

-- B1
promedioSobreUmbral :: Double -> [Double] -> Maybe Double
promedioSobreUmbral = undefined

-- B2
movimientosCaballo :: (Int, Int) -> [(Int, Int)]
movimientosCaballo = undefined

enDosSaltos :: (Int, Int) -> Int
enDosSaltos = undefined

-- B3
miMap :: (a -> b) -> [a] -> [b]
miMap = undefined

miFilter :: (a -> Bool) -> [a] -> [a]
miFilter = undefined

miFoldl :: (b -> a -> b) -> b -> [a] -> b
miFoldl = undefined
