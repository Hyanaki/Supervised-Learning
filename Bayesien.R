# ---------- Chargement des packages nécéssaires
library(e1071)
source("C:/Users/Utilisateur/Desktop/ACI/code/fonctions_utiles.R")
# ---------- Chargement des données
CalPatch = read.table("C:/Users/Utilisateur/Desktop/ACI/img_data/caltech/raw/caltech_colorhist.dat")
CalPatch_test = read.table("C:/Users/Utilisateur/Desktop/ACI/img_data/caltech/raw/caltech_colorhist50.dat")

# ---------- Construction du classifieur Bayesien Naif
# Pour le bayesien naif, il n y a pas de paramètres à ajuster donc on apprend le modèle sur
# l'ensemble d'apprentissage uniquement (pas d'ensemble de validation)
bayes = naiveBayes(V769~., data = CalPatch)

p=predict(bayes, CalPatch_test)
print(p)

bool=(p==CalPatch_test$V769)
print(bool)

accuracy_test=sum(bool)/dim(CalPatch_test)[1]
print(accuracy_test)
print(sum(bool))