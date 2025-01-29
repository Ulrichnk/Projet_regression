
source("functions.R")

#importation des données
train<-read.csv('./data/train_set.csv', header = T, sep = ",",dec=".")
test<-read.csv('./data/test_set.csv', header = T, sep = ",",dec=".")

#appercu des données
head(test)

#données manquantes
sum(is.na(train))
