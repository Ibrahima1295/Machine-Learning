
library(ggplot2)
library(caret)
library(plotROC)
library(kernlab)
library(MASS)
library(klaR)
D=read.csv("online_shoppers_intention.csv", header = TRUE, sep = ",")
head(D)
is.na(D); sum(is.na(D))
str(D)
summary(D)
attach(D)
D = na.omit(D)

#####Partie 1: Analyse des donnees ###############

#1. Analyse des variables quantitatives
##on represente les variables quantitatives sur un seul graphe

#Administrative
ggplot(D, aes(x=Administrative ))+
  geom_bar() +
  facet_grid(Revenue ~ .,
             scales = "free_y")
#Administrative Duration
ggplot(D, aes(x=Administrative_Duration ))+
  geom_histogram(bins = 20)+
  facet_grid(Revenue ~ .,
             scales = "free_y")
#Informational
ggplot(D, aes(x=Informational ))+
  geom_bar() +
  facet_grid(Revenue ~ .,
             scales = "free_y")
#Informational Duration
ggplot(D, aes(x=Informational_Duration ))+
  geom_histogram(bins = 20)+
  facet_grid(Revenue ~ .,
             scales = "free_y")
#ProductRelated
ggplot(D, aes(x=ProductRelated ))+
  geom_bar(bins = 20)+
  facet_grid(Revenue ~ .,
             scales = "free_y")
#ProductRelated_Duration
ggplot(D, aes(x=ProductRelated_Duration ))+
  geom_histogram(bins = 100)+
  facet_grid(Revenue ~ .,
             scales = "free_y")
#BounceRates 
ggplot(D, aes(x=BounceRates ))+
  geom_histogram(bins = 100)+
  facet_grid(Revenue ~ .,
             scales = "free_y")
#ExitRates
ggplot(D, aes(x=ExitRates ))+
  geom_histogram(bins = 100)+
  facet_grid(Revenue ~ .,
             scales = "free_y")
#PageValues
ggplot(D, aes(x=PageValues ))+
  geom_histogram(bins = 100)+
  facet_grid(Revenue ~ .,
             scales = "free_y")
#SpecialDay
ggplot(D, aes(x=SpecialDay ))+
  geom_histogram(bins = 100)+
  facet_grid(Revenue ~ .,
             scales = "free_y")


# Analyse des variables categorielles
#Month
ggplot() +
  aes(x = Month, Revenue = ..count../nrow(D), fill = Revenue) +
  geom_bar() +
  ylab("relative frequency")
###########################################################

######################Partie 2: Implentationtation des modèles#####


######cat?goriser la variable Month et le mettre en numeric
class(Month)#character 
Month<-as.factor(Month) 
levels(Month) 
levels(Month)=c(1:10) 
Month<-as.numeric(Month) 
D$Month = Month
str(Month)

####categoriser la variable visitor type et le mettre en numeric
class(VisitorType)#character 
VisitorType<-as.factor(VisitorType) 
levels(VisitorType) 
levels(VisitorType)=c(1:3) 
VisitorType<-as.numeric(VisitorType) 
D$VisitorType = VisitorType
str(VisitorType)



#####################################
### Sous echantillonnage de D 
m = 12330^(2/3) 
sub=sample(1:12330, size=m, replace=F)

D_sub = D[sub,]
str(D_sub)
D_sub<-data.frame(D_sub)

#### on fait le changement pour la variable Weekend en <<numeric>>
class(D_sub$Weekend)#logical
Weekend<-as.factor(Weekend)
levels(Weekend)
levels(Weekend)=c(1:2)
D_sub$Weekend<-as.numeric(D_sub$Weekend)
D_sub$Weekend = D_sub$Weekend
class(D_sub$Weekend)

#### on fait le changement pour la variable Renvenue en factor>>

class(D_sub$Revenue)#logical
levels(D_sub$Revenue)
levels(D_sub$Revenue)=c(1:2)
str(D_sub$Revenue)
D_sub$Revenue<-as.numeric(D_sub$Revenue)
D_sub$Revenue = D_sub$Revenue
class(D_sub$Revenue)
table(D_sub$Revenue)

D_sub$Revenue <- as.factor(ifelse(D_sub$Revenue == 0, "Non", "Oui"))
D_sub$Revenue <- as.factor(D_sub$Revenue)
table(D_sub$Revenue)

################################1) Methode KNN #############

## Apprentissage/Test
set.seed(300) #fixe la partition 
indxTrain = createDataPartition(y =  D_sub$Revenue,p = 0.75,list = FALSE)
Dtrain = D_sub[indxTrain,]
Dtest =  D_sub[-indxTrain,]

# Methode KNN avec K=5
ctrl=trainControl(method = "none")
# apprentissage par knn
fit.5knn = train( Revenue~ ., data=Dtrain, method="knn",
                  tuneGrid=data.frame(k=5), trControl=ctrl)
pred.5knn=predict(fit.5knn,newdata=Dtest)
xtab=table(pred.5knn,Dtest$Revenue)
mat = confusionMatrix(xtab)
print(mat$table)# pred.5knn FALSE TRUE
#FALSE   109   20
#TRUE      2    2



#Précision ou de taux de bien classés
mat$overall["Accuracy"]  ##0.8345865  
#Taux de mal classés
1-mat$overall["Accuracy"] ##0.1654135 

## On va tester avec K= 1 , K=50 , K=300
###Pour K=1
ctrl=trainControl(method = "none")
# apprentissage par knn
fit.1knn = train( Revenue~ ., data=Dtrain, method="knn",
                  tuneGrid=data.frame(k=1), trControl=ctrl)
pred.1knn=predict(fit.1knn,newdata=Dtest)
xtab=table(pred.1knn,Dtest$Revenue)
mat = confusionMatrix(xtab)
print(mat$table)# pred.1knn FALSE TRUE
#FALSE   103   12
#TRUE      8   10

#Précision ou de taux de bien classés
mat$overall["Accuracy"] ## 0.8496241 Accuracy
#Taux de mal classés
1-mat$overall["Accuracy"]##0.1503759   Taux d'erreur

###Pour K=50
ctrl=trainControl(method = "none")
# apprentissage par knn
fit.50knn = train( Revenue~ ., data=Dtrain, method="knn",
                   tuneGrid=data.frame(k=50), trControl=ctrl)
pred.50knn=predict(fit.50knn,newdata=Dtest)
xtab=table(pred.50knn,Dtest$Revenue)

mat = confusionMatrix(xtab)
print(mat$table)#pred.50knn FALSE TRUE
#pred.50knn FALSE TRUE
#FALSE   111   22
#TRUE      0    0



#Précision ou de taux de bien classés
mat$overall["Accuracy"] ## 0.8345865 Accuracy
#Taux de mal classés
1-mat$overall["Accuracy"]##0.1654135    Taux d'erreur

###Pour K=300
ctrl=trainControl(method = "none")
# apprentissage par knn
fit.300knn = train( Revenue~ ., data=Dtrain, method="knn",
                    tuneGrid=data.frame(k=300), trControl=ctrl)
pred.300knn=predict(fit.300knn,newdata=Dtest)
xtab=table(pred.300knn,Dtest$Revenue)
mat = confusionMatrix(xtab)
print(mat$table)#pred.300knn FALSE TRUE
#FALSE   FALSE   111   22
#TRUE      0    0
#Précision ou de taux de bien classés
mat$overall["Accuracy"] ##0.8345865 Accuracy
#Taux de mal classés
1-mat$overall["Accuracy"]##0.1654135    Taux d'erreur





#

#pour k=1
ntrain = length(indxTrain)
ks = seq(from=1, to= ntrain, by=10)

ks
err.pred = c()
for (n in ks) {
  fit.knn= train(Revenue ~ ., data = Dtrain,method="knn",
                 tuneGrid=data.frame(k=1), trControl=ctrl)
  ## predictions sur l'ensemble de validation
  pred.1knn = predict(fit.1knn, newdata=Dtest[,1:18])
  mat = confusionMatrix(pred.1knn,Dtest[,18])
  ## taux d'erreur de prediction
  err.pred.k = 1-mat$overall["Accuracy"]
  err.pred = c(err.pred,err.pred.k)
}


ajst =c()
err_pred =c()
ks=c(1,50,300)
for(k in ks){
  
  set.seed(300) #fixe la partition 
  indxTrain = createDataPartition(y = D_sub$Revenue,p = 0.75,list = FALSE)
  Dtrain = D_sub[indxTrain,]
  Dtest = D_sub[-indxTrain,]
  
  ## apprentissage par knn    
  ctrl = trainControl(method="none")
  fit.knn = train(Revenue ~ ., data = Dtrain, method="knn",
                  tuneGrid=data.frame(k=k), trControl=ctrl)}

## erreur d'ajustement
Fit = predict(fit.knn)
mat_fit = confusionMatrix(table(Fit, Dtrain$Revenue))
ajst =c(ajst,1-mat_fit$overall["Accuracy"])
ajst ##0.1675

## erreur de prediction
pred.knn = predict(fit.knn, newdata=Dtest)
xtab = table(pred.knn, Dtest$Revenue)
mat = confusionMatrix(xtab)
err_pred=c(err_pred,1-mat$overall["Accuracy"])                      
err_pred
##0.1654135



#pour k=1
ntrain = length(indxTrain)
ks = seq(from=1, to= ntrain, by=10)
ks
err.pred = c()
for (n in ks) {
  fit.knn= train(Revenue ~ ., data = Dtrain,method="knn",
                 tuneGrid=data.frame(k=1), trControl=ctrl)
  ## predictions sur l'ensemble de validation
  pred.1knn = predict(fit.1knn, newdata=Dtest[,1:18])
  mat = confusionMatrix(pred.1knn,Dtest[,18])
  ## taux d'erreur de prediction
  err.pred.k = 1-mat$overall["Accuracy"]
  err.pred = c(err.pred,err.pred.k)
}


ks
err.pred
E = data.frame(x=ks, y=err.pred)
colnames(E)=c("Nbre_voisins", "Taux_prédiction");
ggplot(E, aes(x=Nbre_voisins, y=Taux_prédiction))+
  xlab("Nombre de voisins") +
  ylab("Taux d'erreur de prédiction")+
  geom_point(size=2,color="blue")+
  geom_line(size=0.5,color="blue")+
  geom_hline(yintercept=min(err.pred), linetype="dashed",
             color = "green", size=0.5)
min(err.pred) ##0.06015038

ctrl = trainControl(method="cv",number=10)
knnFit = train(Revenue ~ ., data = Dtrain, method = "knn",
               trControl = ctrl, tuneLength = 160,
               preProcess = c("center","scale"),
               tuneGrid = expand.grid(k = c(1, 50,300)))
# Taux de succes ("Accuracy")
x11();plot(knnFit)
knnFit$results;knnFit$bestTune # meilleur K nn = 50 
## le meilleur K est K=50 .  
## il est le meilleur K parce qu'il a le plus grand accuracy et un

#taux d'erreur plus faible parmis les 2 autres(K=1, K=300)




#################################################################################################
###################### ### 2) Methode Bayesien Naif #############

set.seed(300)
indxTrain = createDataPartition(y = D_sub$Revenue,p = 0.75,list = FALSE)
Dtrain = D_sub[indxTrain,]
Dtest = D_sub[-indxTrain,]
## on sÃ©pare les classes, et implÃ©mente la LDA et la QDA en utilisant la
## les fonction, et puis on a eu 3 apprentissage(l'apprentissage par nb, l'apprentissage par lda et l'apprentissage 
## par qda)
ctrl = trainControl(method="cv", number = 10)
fit.nb = train(Revenue ~ ., data = Dtrain, method="nb",trControl=ctrl)
pred.nb = predict(fit.nb, newdata=Dtest[,1:18],prob=TRUE)
tab = table(pred.nb, Dtest$Revenue)
mat = confusionMatrix(tab)
mat$overall["Accuracy"] ## 0.8120301  
1-mat$overall["Accuracy"] ##0.1879699


###Pour séparer les classes, nous allons implémentez la LDA et la QDA en utilisant la
#fonction :

ctrl = trainControl(method="none")
# apprentissage par lda
fit.lda = train(Revenue ~ ., data = Dtrain, method="lda",
                trControl=ctrl)
pred.lda = predict(fit.lda, newdata=Dtest[,1:18],prob=TRUE)
tab = table(pred.lda, Dtest$Revenue)
mat = confusionMatrix(tab)
mat$overall["Accuracy"] ## 0.8726992 
1-mat$overall["Accuracy"] ##0.1203008

# apprentissage par qda
fit.qda = train(Revenue ~ ., data = Dtrain, method="qda",
                trControl=ctrl)
pred.qda = predict(fit.qda, newdata=Dtest[,1:18],prob=TRUE)
tab = table(pred.qda, Dtest$Revenue)
mat = confusionMatrix(tab)
mat$overall["Accuracy"] ## 0.8646617 
1-mat$overall["Accuracy"] ##0.1212121 

#################################################################"
####################  Comparaison Nb, Lda et Qda ###################


set.seed(300)
indxTrain = createDataPartition(y = D_sub$Revenue,p = 0.75,list = FALSE)
Dtrain = D_sub[indxTrain,]
Dtest = D_sub[-indxTrain,]
ctrl1  = trainControl(method="cv",number=5, classProbs=TRUE,
                     summaryFunction=twoClassSummary,
                     savePredictions = "all" )
CVfit.nb = train (Revenue ~ ., data = Dtrain, method="nb",
                  trControl=ctrl1,metric="ROC")
CVfit.lda = train(Revenue ~ ., data = Dtrain, method="lda",
                  trControl=ctrl1,metric="ROC")
CVfit.qda = train(Revenue ~ ., data = Dtrain, method="qda",
                  trControl=ctrl1,metric="ROC")
## en suite on fait les predictions sur chaque individu des donnees d'apprentissage 
## lorsqu'il appartient Ã  un echantillon de validation lors de la procedure de validation croisee
## grace a l'option savePredictions = "all" dans trainControl

score.nb = CVfit.nb$pred
score.lda = CVfit.lda$pred
score.qda = CVfit.qda$pred
library(plotROC)
score.data = cbind(score.nb$obs,score.nb["Non"],score.lda["Non"],score.qda["Non"])
colnames(score.data) = c("Revenue","nb","lda","qda")
score.data <- melt_roc(score.data,"Revenue",c("nb","lda","qda"))

ggplot(score.data, aes(d =D, m = M, color = name)) + geom_roc()

## en suite on fait les ROC pour les 3 methodes pour comparer lequel est mieux(on trouve que la methode lda predit mieux que les 2 autres)
## donc on fait la prediction pour lda, on a eu l'accuracy = 0.9090909

c(CVfit.nb$results$ROC,CVfit.lda$results$ROC,CVfit.qda$results$ROC)

#predictions pour lda
pred.lda = predict(CVfit.lda,newdata=Dtest)
tab =table(pred.lda, Dtest$Revenue)
mat = confusionMatrix(tab)
mat
mat$overall["Accuracy"]

##################################################################
###################### 3) Modele  SVM ##############
set.seed(300)
indxTrain = createDataPartition(D_sub$Revenue ,p=0.75,list=FALSE)
Dtrain = D_sub[indxTrain,]
Dtest = D_sub[-indxTrain,]

##Separateur Lineaire
ctrl = trainControl(method = "none")
fitLin.svm = train(Revenue~.,data=Dtrain, method="svmLinear",type="C-svc",
                   trControl=ctrl,tuneGrid = data.frame(.C = 0.1))
print(fitLin.svm)


ctrl = trainControl(method="cv",number=5)
svmGrid_lin = seq(0.0001,0.01,by=0.0001)
CVfitLin.svm = train(Revenue~., data=Dtrain,method="svmLinear",
                     type="C-svc",trControl=ctrl,
                     tuneGrid = data.frame(.C = svmGrid_lin))
plot(CVfitLin.svm)
CVfitLin.svm$bestTune # 0.0028
#on peut choisir c=0.0090 à C=0,010 

# le taux de précision sur les données test
pred.CV=predict(CVfitLin.svm,newdata=Dtest)
tab =table(pred.CV, Dtest$Revenue)
mat = confusionMatrix(tab)
print(mat$table)
mat$overall["Accuracy"]# 0.8421053  
1-mat$overall["Accuracy"]# 0.1578947

#Séparateur non linéaire
# méthode SVM avec un noyau Gaussien :

ctrl = trainControl(method = "none")
fitGaus.svm = train(Revenue~., data=Dtrain, method = "svmRadial",
                    trControl = ctrl,
                    tuneGrid = data.frame(.C =0.05, .sigma = 0.1))
print(fitGaus.svm)
##e calibrer les paramètres à l'aide de la validation croisée.
ctrl = trainControl(method="cv",number=5, classProbs=TRUE,
                    summaryFunction=twoClassSummary,
                    savePredictions = "all" )  

svmGrid_gaus = expand.grid(.sigma= 2^c(-25,-20,-15,-10,-5, 0, 5, 10),
                           .C= 2^c(0:5))
CVfitGaus.svm= train(Revenue~., data=Dtrain,method="svmRadial",
                     type="C-svc",trControl=ctrl,
                     tuneGrid = svmGrid_gaus, metric="ROC")
#Representer
trellis.par.set(caretTheme())
plot(CVfitGaus.svm, scales = list(x = list(log = 2)))

pred.CV=predict(CVfitGaus.svm,newdata=Dtest)
tab =table(pred.CV, Dtest$Revenue)
mat = confusionMatrix(tab)
print(mat$table)
mat$overall["Accuracy"]##0.8484848
1-mat$overall["Accuracy"]

#Implémentez de même la méthode SVM avec un noyau polynomial :

ctrl = trainControl(method = "none")

fitPoly.svm = train(Revenue~., data=Dtrain, method = "svmPoly",
                    trControl = ctrl,
                    tuneGrid = data.frame(.C =0.05, .degree=2, .scale=.1))

print(fitPoly.svm)
pred.fitPoly.svm= predict(fitPoly.svm, newdata=Dtest[,1:18],prob=TRUE)
tab1 = table(pred.fitPoly.svm, Dtest$Revenue)
mat1 = confusionMatrix(tab1)
1-mat1$overall["Accuracy"] ##0.1503759
mat1

##e calibrer les paramètres à l'aide de la validation croisée.
ctrl = trainControl(method="cv",number=5, classProbs=TRUE,
                    summaryFunction=twoClassSummary,
                    savePredictions = "all" )
svmGrid_poly = expand.grid(.degree=(2:5), .scale=.1,
                           .C=c(0.01,0.1,1,3,5,10,20,50, 100,200,500))
CVfitPoly.svm= train(Revenue~., data=Dtrain,method= "svmPoly",
                     type="C-svc",trControl=ctrl,
                     tuneGrid = svmGrid_poly, metric="ROC")
trellis.par.set(caretTheme())
plot(CVfitPoly.svm, scales = list(x = list(log = 2)))

pred.CV=predict(CVfitPoly.svm,newdata=Dtest)
tab =table(pred.CV, Dtest$Revenue)
mat = confusionMatrix(tab)
print(mat$table)
mat$overall["Accuracy"]
1-mat$overall["Accuracy"]


#(c) Affichez les paramètres optimaux
CVfitGaus.svm$bestTune
CVfitPoly.svm$bestTune

#############################################

########### 4) Modele Regression Logistic##############

set.seed(300) #fixe la partition 
indxTrain = createDataPartition(y = D_sub$Revenue,p = 0.75,list = FALSE)
Dtrain = D_sub[indxTrain,]
Dtest = D_sub[-indxTrain,]
#parametres de controle
ctrl = trainControl(method="none")
# apprentissage par régression logistique
fit.lr = train(Revenue ~ ., data = Dtrain,method="glm",trControl=ctrl)
print(fit.lr)
# coeffs estimé
print(fit.lr$finalModel)



# Prediction

score.lr = predict(fit.lr, newdata=Dtest, type="prob")
print(score.lr)

# Class prédites

class.lr = predict(fit.lr,newdata=Dtest)
#distribution des classes prédites
table(class.lr)
# Comparez les valeurs de score.lr et class.lr sur quelques individus de Dtest :
list(head(score.lr), head(class.lr))

#### Evaluation du model scoring
#Analyse de la table de confusion

tab = table(data=class.lr,reference=Dtest$Revenue)
mat = confusionMatrix(tab)
print(mat$table)
# 109 séssions se terminant par des non achat ont été prédit non achat
# 12 séssions se terminant par des achats ont été prédit achat vraie TRUE
# 3 séssions se terminant par des achats ont été prédit non achat
#et 8 séssions se terminant par des non achats ont été prédit achat.

mat$byClass[c("Sensitivity", "Specificity")]
# le taux de vrais négatifs : spécificité 0.9910714 
#le taux de vrais positifs : sensibilité 0.3333333
###################################################

#comparaison avec K-NN
ctrl = trainControl(method="none",
                    classProbs=TRUE,
                    summaryFunction=twoClassSummary)
fit.knn = train(Revenue ~ ., data = Dtrain, method = "knn",
                trControl = ctrl, tuneGrid=data.frame(k=50),
                preProcess = c("center","scale"),metric="ROC")
fit.lr = train(Revenue ~ ., data = Dtrain, method = "glm",
               trControl = ctrl,
               preProcess = c("center","scale"),metric="ROC")
#score.knn et score.lr sont les sorties
score.knn = predict(fit.knn ,newdata= Dtest,type="prob")
score.lr = predict(fit.lr ,newdata= Dtest,type="prob")
#Affichez les courbes ROC issues de l¡¯algorithme k-NN et de la r¨¦gression logistique
#type : le type de mail
#Dtest$type : classe observation, score.knn et score.lr sont predictions
library(plotROC)
score.data = cbind(Dtest$Revenue,score.knn,score.lr)
colnames(score.data) = c("type.test","knn","logit")
score.data <- melt_roc(score.data,"type.test",c("knn","logit"))
g=ggplot(score.data, aes(m = M,d = D,color = name)) + geom_roc()
g

#Comparez l¡¯indice AUC des deux modeles
print(calc_auc(g)$AUC)
print(fit.knn);print(fit.lr)
#pour le moyen de KNN la complexite est le nbre de voisin (K), 
#pour la RL c est le nbre de variable caracteristique
