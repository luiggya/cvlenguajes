{-# LANGUAGE NoMonomorphismRestriction #-}
--Importo las librerias que voy a utilizar, por ejemplo de Data.Char uso una funcion para validar si los ingresado por el usuario
--es un numero o no
import Data.List (sortBy, groupBy, minimumBy,partition,delete,intercalate)
import Data.Ord (comparing)
import Data.Char
import Control.Arrow ((***),second,first)
import Data.Function (on)
import System.IO
import System.Random

--defino tipo Codigo que es una lista de enteros
type 	Codigo 	= 	[Int]
--defino tipo Intento para generar el lanzamiento
type 	Intento	=	[Int]
--defino tipo Aciertos como tupla de enteros que representan los acierto, el primero acierto total, el segundo acierto solo de color
type	Aciertos=	(Int, Int)

--Defino funcion codigo que recibe un entero y devuelve Codigo
--El codigo sera colocado en una lista infinita con valores aleatorios de 1 al 6 y generados por la semilla x que es el parametro de entrada
codigo :: Int -> Codigo
codigo x = randomRs (1,6) (mkStdGen x)

--definicion de funcion generadora de intentos, es la generadora de la población del algoritmo genetico
intento :: Bool -> [Int]
intento x = if (x == True) then [1] else [0] --esta logica es solo para que compile sera desechada

--definicion de funcion de respuestas a los intentos, es la funcion de costos del algoritmo genetico
aciertos :: Intento -> Aciertos
aciertos x = (head x, 0)

--genera un aleatorio entero y no una lista
--basado en la semilla b
--Los aleatorios en HK siempre son los mismos para la misma semilla
aleatorio	:: Int -> Int -> Int
aleatorio a b = (randomRs(1,a)(mkStdGen b))!!(mod b a)

--Funcion que convierte a String una lista de enteros
--Esto es util al momento de guardar en el archivo pues la lista generada de aleatorios no puede ser almacenada como entero
convertirToString :: [Int] -> String
convertirToString [] = ""
convertirToString x = show (head x) ++ convertirToString(tail x)

--Funcion para convertir una lista de char o String a Entero
--Se usa all isNumber x para validar toda la lista de entrada como un todo e indicar si es numero o no. 
--Esta funcion devuelve 0 si el String es vacio [] 
convertirToInt :: String -> Int
convertirToInt [] = 0
convertirToInt x = if (all isNumber x) then read x else 0

--Funcion que crea un archivo si no existe llamado registro.txt y graba en el lo que pasa en el juego. 
--La definicion permite pasar cualquier cosa x y agregarla al fichero por el final, junto con un salto de linea
registrar :: String -> IO()
registrar x = appendFile "registro.txt" (x ++ "\n")

--Es la funcion principal del juego en otras palabras es el corazon de nuestro juego
main :: IO()
main = do
		putStrLn "<< .......SUPER MASTERMIND.......  >>"
		putStrLn "-- Realizado: por Luiggy Allauca   --"
		putStrLn "-------------------------------------"
		putStrLn "Ingrese un número para seleccionar el código secreto aleatorio, x para salir" 
		--Aqui espero que el usuario ingrese un numero que es el generador o semilla de los aleatorios. Como sabemos en haskell los aleatorios para la misma
		--semilla son los mismos asi que se puede comprobar en el archivo guardado que si presiono dos veces el mismo numero se generará el mismo codigo
		a <- getLine	
		--La salida del programa es x como puede verse en el if
		if a /= "x" && a /= "X" then do
			--aleatorios es una lista infinita que es similar al codigo, pero sin los primeros 121 numeros, podrian ser 1 o 10000 			
			let aleatorios = drop 121 (codigo (convertirToInt a))
			--la funcion convertirToString convierte solo 4 elementos de la lista infinita aleatorios el resto los ignora
			--grabo en el archivo el mensaje Codigo secreto y el codigo generado de 4 numeros de la forma 4527
			registrar ("Código secreto: " ++ convertirToString ( take 4 (aleatorios)))
			 		
			main --repetimos el juego hasta que presione x
		else do
			putStrLn "Ha salido del sistema"  -- presiono x 
			
