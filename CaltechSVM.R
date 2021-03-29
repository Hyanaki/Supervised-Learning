# Installation package pour libSVM
# install.packages('e1071')
library(e1071)

# ----- Chargement des fonctions utiles
source("C:/Users/Utilisateur/Desktop/ACI/code/fonctions_utiles.R")


# ---------- Chargement des données
CalPatch = read.table("C:/Users/Utilisateur/Desktop/ACI/img_data/skin_segmentation/skin_nonskin.dat")
CalPatch_test = read.table("C:/Users/Utilisateur/Desktop/ACI/img_data/caltech/raw/caltech_gist50.dat")



### Separation apprentissage / validation / test

nall = nrow(CalPatch) #total number of rows in data

ntrain = floor(0.65 * nall) # number of rows for train: 65% 
nvalid = floor(0.35 * nall) # number of rows for valid: 35%

set.seed(20) # choix d une graine pour le tirage aléatoire
index = sample(nall) # permutation aléatoire des nombres 1, 2, 3 , ... nall

CalPatch_app = CalPatch[index[1:ntrain],] # création du jeu d'apprentissage
CalPatch_val= CalPatch[index[(ntrain+1):(ntrain+nvalid)],] # création du jeu de validation

table(CalPatch_app$V961)

accuracy = 0;
c = 0.0000001;
#Test exhaustif pour tous les param�tres possibles pour c, pour le type de noyau, pour gamma et pour le degr�
while (c<1000) {
  print('c')
  print(c)
  svm_linear<- svm(V961~.,data = CalPatch_app, kernel='linear',cost=c,type='C-classification', scale = T)
  p = predict(svm_linear, CalPatch_val)
  bool=(p==CalPatch_val$V961)
  accuracynew=sum(bool)/dim(CalPatch_val)[1];
    if(accuracynew > accuracy)
  {
    accuracy = accuracynew;
    best_model = svm_linear;
    print('plusplus')
    
    }
  g=0.0000001
  while (g <10) {
    
    svm_radial<- svm(V961~.,data = CalPatch_app, kernel='radial',cost=c,type='C-classification', gamma = g, scale = T)
    p = predict(svm_radial, CalPatch_val)
    bool=(p==CalPatch_val$V961)
    accuracynew=sum(bool)/dim(CalPatch_val)[1]
    if(accuracynew > accuracy)
    {
      accuracy = accuracynew;
      best_model = svm_radial;
      print('supr')
    }
    g=g*10
  }
  for( d in seq(2, 5, by=1)){
    for(coef in seq (0.1,4, by=0.5)){
    svm_poly <- svm(V961~.,data = CalPatch_app, kernel='polynomial',cost=c,type='C-classification', degree = d, coef0 = coef, scale = T)
    p = predict(svm_poly, CalPatch_val)
    bool=(p==CalPatch_val$V961)
    accuracynew=sum(bool)/dim(CalPatch_val)[1]
    if(accuracynew > accuracy)
    {
      accuracy = accuracynew;
      best_model = svm_poly;
      print('sup')
    }
    
  
  }
  }
   
    c=c*10
   
}

p = predict(best_model, CalPatch_test)
print(p)
bool=(p==CalPatch_test$V961)
print(bool)
accuracy_test=sum(bool)/dim(CalPatch_test)[1]

# matrice de confusion
table(p, CalPatch_test$V961)
summary(best_model)
print(accuracy_test)