import System.IO
import Data.Char ()

-- Création d'un type spécial pour les instructions
data Instruction = Forward Float | Left Float | Right Float | Repeat Int [Instruction] deriving    (Read, Show)

-- fonction qui envoie un tuple de position (x, y) en fonction de la distance qu'il doit parcourir et de l'angle courant
move :: Float -> Float -> (Float, Float)
move s l = (x, y)
     where a = s * (pi/180) --l'angle courant ne change pas et conversion de degrés en radian
           x = l * cos a
           y = l * sin a

-- fonction qui met à jour l'angle courant
angle :: Float -> Float -> Float
angle s val = s + val


-- fonction qui ajoute l'ensemble des instructions pour en faire une liste de tuples (x, y) des coordonnées de déplacement
add :: [Instruction] -> [(Float, Float)] -> Float -> [(Float, Float)]
add [] out _ = out --lorsque la liste d'instruction est vide

add (x:xs) out s = case x of
    (Main.Forward i) -> add xs (out ++ [move s i]) s --ajoute un nouveau vecteur sous la forme d'un tuple (x, y)
    
    (Main.Left i) -> add xs out (angle s (-i)) --ajoute l'angle courant précédent à l'angle fourni après l'instruction Left (en l'orientant)
    
    (Main.Right i) -> add xs out (angle s i) --ajoute l'angle courant précédent à l'angle fourni après l'instruction Right (en l'orientant)
    
    (Main.Repeat i j) -> add rep out s
    -- on ajoute au début de la liste des instructions i fois les instructions extraites du repeat
        where rep = (take ((length j) * i) (cycle j)) ++ xs

-- fonction qui retourne une liste de traits au format xml/svg au format liste de String
-- Cette fonction prends en entrée la liste des deplacements du crayon restants à effectuer, la position courante du crayon (cx,cy), et la liste des traits déjà ecrits au format SVG
lgSvg :: [(Float,Float)] -> (Float,Float) -> [[Char]] -> [[Char]]
lgSvg [] _ _ = []
lgSvg (x:xs) (cx,cy) _ = 
   t ++ lgSvg xs (nx,ny) t  -- Création du point suivant en ajoutant le point précédant avec le vecteur suivant
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
     writeFile "SVG.txt" (buildfile (decomplist (lgSvg (add (read texte :: [Instruction]) [] 0) (250, 250) [""]) ""))
