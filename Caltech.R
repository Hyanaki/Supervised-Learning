library(rpart)
library(rpart.plot)

source("C:/Users/Utilisateur/Desktop/ACI/code/fonctions_utiles.R")

# ---------- Chargement des données
CalPatchArbre = read.table("C:/Users/Utilisateur/Desktop/ACI/img_data/caltech/raw/caltech_gist.dat")
CalPatchArbre_test = read.table("C:/Users/Utilisateur/Desktop/ACI/img_data/caltech/raw/caltech_gist50.dat")

table(CalPatchArbre_test$V961)

### Separation apprentissage / validation / test

nall = nrow(CalPatchArbre) #total number of rows in data

# Une s�paration correcte en ensembles d'apprentissage et de test se fait par s�lection al�atoire,
# notamment au cas o�� les donn�es seraient tri�es par valeur de classe, afin d'�viter d'avoir tous les exemples d'une m�me classe 
# dans l'ensemble d'apprentissage et les exemples de l'autre classe dans l'ensemble de test.

ntrain = floor(0.65 * nall) # number of rows for train: 65% 
nvalid = floor(0.35 * nall) # number of rows for valid: 35%


set.seed(20) # choix d une graine pour le tirage aléatoire
index = sample(nall) # permutation al�atoire des nombres 1, 2, 3 , ... nall

CalPatchArbre_app = CalPatchArbre[index[1:ntrain],] # cr�ation du jeu d'apprentissage
CalPatchArbre_val= CalPatchArbre[index[(ntrain+1):(ntrain+nvalid)],] # cr�ation du jeu de validation

table(CalPatchArbre_app$V961) #On v�rifie que l'on a bien une r�partition al�atoire dans notre jeu d'apprentissage

# ---------- Construction de l'arbre

# La fonction rpart permet d'ajuster un arbre de d�cision � partir d'un ensemble d'apprentissage.
# Les paramètres sont :
# - une formule  : NomdeColonne~., data = ... NomDeColonne correspond au nom de la colonne qui contient la classe
# dans les données (ici Reussite), et on doit mettre le nom de la variable qui contient les données après data = 
# - control = list(minbucket = 1,cp = 0, minsplit = 1) ces paramètres signifient que l'on souhaite construire l'arbre entièrement (sans aucun élagage). On verra tout à l'heure comment élaguer

tr = rpart(V961~., data = CalPatchArbre_app, control = list(minbucket = 1,cp = 0, minsplit = 1))

#Affichage de l'arbre
rpart.plot(tr, extra = 1)


# On pr�dit la classe de tous les individus de l'ensemble d'apprentissage et on l'affiche
p=predict(tr, CalPatchArbre_val, type = "class")
print(p)


#Calcul de nombre de pr�diction correcte et affichage des valeurs vraies ou fausses
bool=(p==CalPatchArbre_val$V961)
print(bool)

accuracy=sum(bool)/dim(CalPatchArbre_val)[1]
print(accuracy)

# matrice de confusion
table(p, CalPatchArbre_val$V961)



# ---------- Elagage de l'arbre 
#On cherche l'arbre �lagu� optimal entre performance et complexit�
x = rev(tr$cptable[,1])
best_tr = tr

for (i in x) {
  
  #Elaguage 
  tr_elague = prune(tr, cp = i) # si on met cp = 0, on ne fait aucun �lagage.
 
  # On pr�dicte la classe de tous les individus de l'ensemble de validation
  p=predict(tr_elague, CalPatchArbre_val, type = "class")
  
  # On calcule le nombre de classe correctement pr�dite
  bool=(p==CalPatchArbre_val$V961)
  accuracy_elague=sum(bool)/dim(CalPatchArbre_val)[1]
  print(accuracy_elague)
  
  if(accuracy_elague > accuracy)
  {
    accuracy = accuracy_elague;
    best_tr = tr_elague;
  }
  
}

rpart.plot(best_tr, extra = 1) # Cela donne le m�me arbre que tr

p=predict(tr, CalPatchArbre_val, type = "class")
print(p)

bool=(p==CalPatchArbre_val$V961)
print(bool)
print(accuracy)
# matrice de confusion
table(p, CalPatchArbre_val$V961)

#Test

p=predict(best_tr, CalPatchArbre_test, type = "class")
print(p)

# Calcul les pr�diction correctes sur l'ensemble de test
bool=(p==CalPatchArbre_test$V961)
print(bool)
accuracy_test=sum(bool)/dim(CalPatchArbre_test)[1]
print(accuracy_test)
# matrice de confusion
table(p, CalPatchArbre_test$V961)
