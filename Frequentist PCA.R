#pca-----
res.pca <- prcomp(data[,3:12], scale = TRUE)
fviz_eig(res.pca)
res.pca$rotation #select variable #8,#10,#2

#model training-----
#use dominated variable to fit model
#fit logistic model
model_fitted_pca=glm(trainData$diagnosis~trainData$concavepoints.mean+
                       trainData$fractaldimension.mean+
                       trainData$texture.mean,family = "binomial")
summary(model_fitted_pca)

#model prediction-----
beta.pca=t(c(-3.82807,141.48853,-156.51939,0.31359))
beta.matrix.pca=matrix(beta.pca)
#for test data
X.vec.pca=c(rep(1,nrow(testData)),testData$concavepoints.mean,
            testData$fractaldimension.mean,
            testData$texture.mean)  
X.matrix.pca=matrix(X.vec.pca,byrow = FALSE,ncol = 4)
Z.pca=X.matrix.pca %*% beta.matrix.pca
P.pca=exp(Z.pca)/(1+exp(Z.pca))

#model evaluation-----
testData_pca = cbind(testData,P.pca)
#true negative
nrow(testData_pca[which(
  testData_pca$diagnosis==0&testData_pca$P.pca<0.5),])
#false positive
nrow(testData_pca[which(
  testData_pca$diagnosis==0&testData_pca$P.pca>=0.5),])
#false negative
nrow(testData_pca[which(
  testData_pca$diagnosis==1&testData_pca$P.pca<0.5),]) 
#true positive
nrow(testData_pca[which(
  testData_pca$diagnosis==1&testData_pca$P.pca>=0.5),]) 
