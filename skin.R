#install.packages("png")
#install.packages("jpeg")
library(png)
library(jpeg)
library(rpart)
library(rpart.plot)


##############
# APPRENTISSAGE D'UN MODELE
##############
# ---------- Chargement des packages n√©c√©ssaires
library(e1071)
source("C:/Users/Utilisateur/Desktop/ACI/code/fonctions_utiles.R")
# ---------- Chargement des donn√©es
Skin = read.table("C:/Users/Utilisateur/Desktop/ACI/img_data/skin_segmentation/skin_nonskin.dat")
head(Skin)
str(Skin)
table(Skin$V4)

### Separation apprentissage / validation / test

nall = nrow(Skin) #total number of rows in data

ntrain = floor(0.55 * nall) # number of rows for train: 55% 
nvalid = floor(0.25 * nall) # number of rows for valid: 25%
ntest = floor(0.20 * nall) #number of rows for test : 20%


set.seed(153) # choix d une graine pour le tirage al√©atoire
index = sample(nall) # permutation al√©atoire des nombres 1, 2, 3 , ... nall

Skin_app = Skin[index[1:ntrain],] # cr√©ation du jeu d'apprentissage
Skin_app$V4=as.factor(Skin_app$V4)
Skin_val= Skin[index[(ntrain+1):(ntrain+nvalid)],] # cr√©ation du jeu de validation
Skin_val$V4= as.factor(Skin_val$V4)
Skin_test= Skin[index[(ntrain+nvalid+1):(ntrain+ntest+nvalid)],] # cr√©ation du jeu de validation
Skin_test$V4= as.factor(Skin_test$V4)
tr <- rpart(V4~., data = Skin_app)
#Affichage de l'arbre
rpart.plot(tr, extra = 1)

x = rev(tr$cptable[,1])
best_tr = tr

for (i in x) {
  
  #Elaguage 
  tr_elague = prune(tr, cp = i) # si on met cp = 0, on ne fait aucun Èlagage.
  
  # On prÈdicte la classe de tous les individus de l'ensemble de validation
  p=predict(tr_elague, Skin_val, type = "class")
  
  # On calcule le nombre de classe correctement prÈdite
  bool=(p==Skin_val$V4)
  accuracy_elague=sum(bool)/dim(Skin_val)[1]
  print(accuracy_elague)
  
  if(accuracy_elague > accuracy)
  {
    accuracy = accuracy_elague;
    best_tr = tr_elague;
  }
  
}
# On prÈdit la classe de tous les individus de l'ensemble d'apprentissage et on l'affiche
p=predict(best_tr, Skin_test, type = "class")


#Calcul de nombre de prÈdiction correcte et affichage des valeurs vraies ou fausses
bool=(p==Skin_test$V4)
print(bool)

accuracy=sum(bool)/dim(Skin_test)[1]
print(accuracy)

# matrice de confusion
#table(p,Skin_test$V4)


##############
# TEST DU MODELE
##############
# sur une image

# charger l'image a tester sous forme de 'array'
# Attention: readJPEG ou readPNG normalisent les valeurs des pixels entre 0 et 1
img <- readJPEG("C:/Users/Utilisateur/Desktop/visagerougeur.jpg")

row = dim(img)[1]
col = dim(img)[2]

# matrice pour stocker resultat de classification pour chaque pixel de l'image test
mat <- matrix(NA, nrow = row, ncol = col )

class(img[1,1,])
### TODO
# Boucle pour tester la classe de chaque pixel de l'image test

for(i in seq(1,row)){
  for(j in seq(1,col)){
    pix <- img[i,j,]
   result<-data.frame(V1=round(pix[3]*255), V2=round(pix[2]*255), V3= round(pix[1]*255))
   p=predict(tr, result, "class")
   if(p==2){
     mat[i,j]<-0;
   }
   else{
     mat[i,j]<-1;
   }
   
  }
}


  
# Attention: writePNG prend une matrice dont les valeurs sont normalisees entre 0 et 1
writePNG(mat,"C:/Users/Utilisateur/Desktop/test_image1_res.png")
