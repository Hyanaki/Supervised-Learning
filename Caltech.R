library(rpart)
library(rpart.plot)

source("C:/Users/Utilisateur/Desktop/ACI/code/fonctions_utiles.R")

# ---------- Chargement des donnÃ©es
CalPatchArbre = read.table("C:/Users/Utilisateur/Desktop/ACI/img_data/caltech/raw/caltech_gist.dat")
CalPatchArbre_test = read.table("C:/Users/Utilisateur/Desktop/ACI/img_data/caltech/raw/caltech_gist50.dat")

table(CalPatchArbre_test$V961)

### Separation apprentissage / validation / test

nall = nrow(CalPatchArbre) #total number of rows in data

# Une séparation correcte en ensembles d'apprentissage et de test se fait par sélection aléatoire,
# notamment au cas où¹ les données seraient triées par valeur de classe, afin d'éviter d'avoir tous les exemples d'une même classe 
# dans l'ensemble d'apprentissage et les exemples de l'autre classe dans l'ensemble de test.

ntrain = floor(0.65 * nall) # number of rows for train: 65% 
nvalid = floor(0.35 * nall) # number of rows for valid: 35%


set.seed(20) # choix d une graine pour le tirage alÃ©atoire
index = sample(nall) # permutation aléatoire des nombres 1, 2, 3 , ... nall

CalPatchArbre_app = CalPatchArbre[index[1:ntrain],] # création du jeu d'apprentissage
CalPatchArbre_val= CalPatchArbre[index[(ntrain+1):(ntrain+nvalid)],] # création du jeu de validation

table(CalPatchArbre_app$V961) #On vérifie que l'on a bien une répartition aléatoire dans notre jeu d'apprentissage

# ---------- Construction de l'arbre

# La fonction rpart permet d'ajuster un arbre de décision à partir d'un ensemble d'apprentissage.
# Les paramÃ¨tres sont :
# - une formule  : NomdeColonne~., data = ... NomDeColonne correspond au nom de la colonne qui contient la classe
# dans les donnÃ©es (ici Reussite), et on doit mettre le nom de la variable qui contient les donnÃ©es aprÃ¨s data = 
# - control = list(minbucket = 1,cp = 0, minsplit = 1) ces paramÃ¨tres signifient que l'on souhaite construire l'arbre entiÃ¨rement (sans aucun Ã©lagage). On verra tout Ã  l'heure comment Ã©laguer

tr = rpart(V961~., data = CalPatchArbre_app, control = list(minbucket = 1,cp = 0, minsplit = 1))

#Affichage de l'arbre
rpart.plot(tr, extra = 1)


# On prédit la classe de tous les individus de l'ensemble d'apprentissage et on l'affiche
p=predict(tr, CalPatchArbre_val, type = "class")
print(p)


#Calcul de nombre de prédiction correcte et affichage des valeurs vraies ou fausses
bool=(p==CalPatchArbre_val$V961)
print(bool)

accuracy=sum(bool)/dim(CalPatchArbre_val)[1]
print(accuracy)

# matrice de confusion
table(p, CalPatchArbre_val$V961)



# ---------- Elagage de l'arbre 
#On cherche l'arbre élagué optimal entre performance et complexité
x = rev(tr$cptable[,1])
best_tr = tr

for (i in x) {
  
  #Elaguage 
  tr_elague = prune(tr, cp = i) # si on met cp = 0, on ne fait aucun élagage.
 
  # On prédicte la classe de tous les individus de l'ensemble de validation
  p=predict(tr_elague, CalPatchArbre_val, type = "class")
  
  # On calcule le nombre de classe correctement prédite
  bool=(p==CalPatchArbre_val$V961)
  accuracy_elague=sum(bool)/dim(CalPatchArbre_val)[1]
  print(accuracy_elague)
  
  if(accuracy_elague > accuracy)
  {
    accuracy = accuracy_elague;
    best_tr = tr_elague;
  }
  
}

rpart.plot(best_tr, extra = 1) # Cela donne le même arbre que tr

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

# Calcul les prédiction correctes sur l'ensemble de test
bool=(p==CalPatchArbre_test$V961)
print(bool)
accuracy_test=sum(bool)/dim(CalPatchArbre_test)[1]
print(accuracy_test)
# matrice de confusion
table(p, CalPatchArbre_test$V961)
