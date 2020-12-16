from math import *
from pylab import *

LS = open("/home/elisa/Documents/TC3/ELP/HASKELL/Projeeet/logoskell.txt", "r")


def FtLm (t) :     #Crée une liste ordonnée composée des mots et des crochets du fichier
    L = []     #initialisation de la liste
    k = ""     #Cumulateur de lettres pour former un mot
    x = 0    #Pour eviter les '' dans la liste
    lignes = t.readlines()       #Crée une liste composée des lignes du fichier
    for i in lignes :
        for j in i :
            if j == " " or j == ',' :
                if x == 0 :            #Verifie si le caractère précédent n'est pas un espace ou une virgule pour ne pas ajouter de '' à la liste
                    L.append(k)        #ajout du mot créé à la liste
                k = ""             #Réinitialisation du cumulateur
                x += 1             #Maque qu'un espace ou une virgule à été parcouru
            elif j == "[" :
                L.append(j)
                x = 0               #Réinitialisation du marqueur d'espace/virgule
            elif j == "]" :
                L.append(k)
                L.append(j)
                k = ""
                x = 1               #Pour éviter d'ajouter un '' à la liste
            else :
                k += j
                x = 0
    return L


def instruction (i,X,Y,L,T) :
    if L[i] == "Forward" :
        d = int(L[i+1])
        n = len(X)-1
        p = 1000000         #pour permettre de contrer les erreurs d'arrondis de python
        X.append(round((X[n]+d*cos(T))*p)//p)   #trigo basées sur le graphe fourni en exemple
        Y.append(round((Y[n]-d*sin(T))*p)//p)
    elif L[i] == "Left" :
        T += int(L[i+1])*pi/180
    elif L[i] == "Right" :
        T += -int(L[i+1])*pi/180
    elif L[i] == "Repeat" :
        for k in range (int(L[i+1])) :
            c = 1
            while L[i+c] != "]" :
                T = instruction (i+c,X,Y,L,T)
                c += 1
    return T

def pos (t) :
    X = [0]     #Absisses
    Y = [0]     #Ordonné
    T = 0       #Angle
    L = FtLm (t)
    for i in range (len(L)) :
        instruction (i,X,Y,L,T)
    return X,Y

X,Y = pos(LS)
print (X,Y)


SVG = open("/home/elisa/Documents/TC3/ELP/HASKELL/Projeeet/logoskell.txt", "w")

SVG.write('<?xml version="1.0" encoding="utf-8"?><svg xmlns="http://www.w3.org/2000/svg" version="1.1" width="200" height="200">\n<title>Exemple</title>\n')


for i in range (len(X)-1) :
    SVG.write('<line x1='+str(X[i])+' y1='+str(Y[i])+' x2='+str(X[i+1])+' y2='+str(Y[i+1])+' stroke="red" />\n')

SVG.write('</svg>')

plot(X,Y)  #Affichage du graphe via matplotlib

LS.close()
SVG.close()

show()