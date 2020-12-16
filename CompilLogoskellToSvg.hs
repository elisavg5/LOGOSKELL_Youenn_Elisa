import Data.Char
import System.IO



--Déclaration de la liste d'instruction 
data Instructions = Forward Int | Right Int | Left Int | Repeat Int [Instructions] deriving (Show, Read)

--Déclaration de l'instruction Forward
forward :: Int -> Int -> (Int, Int)
forward a b = (x, y)
    where c = a * cos(pi/180)
          x = b * cos c
          y = b * sin c

--Déclaration de l'instruction turn
turn :: Int -> Int -> Int
turn x y = val
    where val = x + y
    
--Création des tuples Instruction Int
tup :: [Instructions] -> [(Int, Int)] -> Int -> [(Int, Int)]
tup [] ext = ext

--tup (inst:x) ext n = case inst of
--   (Main.Forward i) -> tup 

main = do read <- readFile "logoskell.txt"
	  

          


