#install.packages("png")
#install.packages("jpeg")
library(png)
library(jpeg)
library(rpart)
library(rpart.plot)


##############
# APPRENTISSAGE D'UN MODELE
##############
# ---------- Chargement des packages nécéssaires
library(e1071)
source("C:/Users/Utilisateur/Desktop/ACI/code/fonctions_utiles.R")
# ---------- Chargement des données
Skin = read.table("C:/Users/Utilisateur/Desktop/ACI/img_data/skin_segmentation/skin_nonskin.dat")
head(Skin)
str(Skin)
table(Skin$V4)

### Separation apprentissage / validation / test

nall = nrow(Skin) #total number of rows in data

ntrain = floor(0.65 * nall) # number of rows for train: 65% 
ntest = floor(0.35 * nall) # number of rows for valid: 35%



set.seed(20) # choix d une graine pour le tirage aléatoire
index = sample(nall) # permutation aléatoire des nombres 1, 2, 3 , ... nall

Skin_app = Skin[index[1:ntrain],] # création du jeu d'apprentissage
Skin_app$V4=as.factor(Skin_app$V4)
Skin_test= Skin[index[(ntrain++1):(ntrain+ntest)],] # création du jeu de validation
Skin_test$V4= as.factor(Skin_test$V4)



svm_linear<- svm(V4~.,data = Skin_app, kernel='linear',cost=0.01,type='C-classification', scale = T)

p = predict(svm_linear, Skin_test)
print(p)
bool=(p==Skin_test$V4)
print(bool)
accuracy_test=sum(bool)/dim(Skin_test)[1]

# matrice de confusion
table(p, Skin_test$V4)
summary(svm_linear)
print(accuracy_test)
# matrice de confusion
#table(p,Skin_test$V4)


##############
# TEST DU MODELE
##############
# sur une image

# charger l'image a tester sous forme de 'array'
# Attention: readJPEG ou readPNG normalisent les valeurs des pixels entre 0 et 1
img <- readJPEG("C:/Users/Utilisateur/Desktop/visage.jpg")

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
    p=predict(svm_linear, result)
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
