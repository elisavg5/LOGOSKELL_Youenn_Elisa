import System.IO
import Control.Monad ()
import Data.Char ()

-- Création d'un type spécial pour les instructions
data Instruction = Forward Float | Left Float | Right Float | Repeat Int [Instruction] deriving    (Read, Show)

-- fonction qui envoie un tuple de position (x, y) en fonction de la distance qu'il doit parcourir et de l'angle courant
forward :: Float -> Float -> (Float, Float)
forward s l = (x, y)
     where a = s * (pi/180) --l'angle courant ne change pas et conversion de degrés en radian
           x = l * cos a
           y = l * sin a

-- fonction qui met à jour l'angle courant
turn :: Float -> Float -> Float
turn s val = s + val


-- fonction qui compute l'ensemble des instructions pour en faire une liste de tuples (x, y) des coordonnées de déplacement
compute :: [Instruction] -> [(Float, Float)] -> Float -> [(Float, Float)]
compute [] out _ = out --lorsque la liste d'instruction est vide

compute (x:xs) out s = case x of
    (Main.Forward i) -> compute xs (out ++ [forward s i]) s --prend le point (x, y) précédent et lui ajoute le tuple donné par notre instruction forward
    
    (Main.Left i) -> compute xs out (turn s (-i)) --ajoute l'angle courant précédent à l'angle fourni après l'instruction Left (en l'orientant)
    
    (Main.Right i) -> compute xs out (turn s i) --ajoute l'angle courant précédent à l'angle fourni après l'instruction Right (en l'orientant)
    
    (Main.Repeat i j) -> compute newXs out s
    -- on ajoute au début de la liste des instructions i fois les instructions extraites du repeat
        where newXs = (take ((length j) * i) (cycle j)) ++ xs

-- fonction qui retourne une liste de traits au format xml/svg au format liste de String
-- Cette fonction prends en entrée la liste des deplacements du crayon restants à effectuer, la position courante du crayon (cx,cy), et la liste des traits déjà ecrits au format SVG
logoskell2svg :: [(Float,Float)] -> (Float,Float) -> [[Char]] -> [[Char]]
logoskell2svg [] _ _ = []
logoskell2svg (x:xs) (cx,cy) s =
   t ++ logoskell2svg xs (nx,ny) t
   where nx = cx + fst x
         ny = cy + snd x
         t = [concat["<line x1=\"", show(cx) , "\" y1=\"", show(cy),"\" x2=\"", show(nx), "\" y2=\"", show(ny), "\" stroke=\"red\" />\n"]]

-- fonction qui transforme la liste des traits au format liste de String,  en une seule String 
decomplist :: [[Char]] -> [Char] -> [Char]
decomplist [] a = a
decomplist (x:xs) s = 
    s ++ x ++ decomplist xs s 

-- fonction qui ajoute l'en-tete et le pied de page au fichier pour respecter la syntaxe xml à la sting contenant l'ensemble des traits
buildfile :: [Char] -> String
buildfile a = "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<svg xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\" width=\"500\" height=\"500\">\n<title>Exemple</title>\n"++a++"</svg>"        

main = do
     --Recuperation des instructions logoskell dans une variable
     texte <- readFile "logoskell.txt"
     
     -- composition de toutes les précédentes fonctions 
     hPutStr stdout (buildfile (decomplist (logoskell2svg (compute (read texte :: [Instruction]) [] 0) (250, 250) [""]) ""))
