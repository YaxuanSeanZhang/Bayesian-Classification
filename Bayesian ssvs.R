#variable selection-----
modelstring_ssvs <- "
model{

for(i in 1:N){
   
   logit(prob[i]) <- alpha + beta[1]*x[i,1] + beta[2]*x[i,2] + beta[3]*x[i,3] + 
      beta[4]*x[i,4] + beta[5]*x[i,5] + beta[6]*x[i,6] + beta[7]*x[i,7] + 
      beta[8]*x[i,8] + beta[9]*x[i,9] + beta[10]*x[i,10] 
   
   y[i] ~ dbern(prob[i])
}

alpha ~ dnorm(0,0.0001)

for(j in 1:10){
  beta[j] ~ dnorm(0,prec[j])
  prec[j] <- 1/((1-gamma[j])*0.0001 + gamma[j]*100)
  gamma[j] ~ dbern(0.5)
}
}
"

dir <- "Models"
modelfile_ssvs <- file.path(dir,"project_ssvs.txt")
writeLines(modelstring_ssvs,modelfile_ssvs)

dat_ssvs <- list(N=nrow(data),
                 x=data[,3:12],
                 y=data$diagnosis)

jagsModel_ssvs <- jags.model(file=modelfile_ssvs, 
                             data=dat_ssvs,
                             n.chains = 2, n.adapt = 1000)

# Use the function 'update' for burnin iterations
update(jagsModel_ssvs, n.iter=1000)

# Now run MCMC and collect posterior samples in coda format
codaSamples_ssvs <- coda.samples(model=jagsModel_ssvs, 
                                 variable.names = c("alpha","beta","gamma"), 
                                 n.iter = 10000, thin = 1)
summary(codaSamples_ssvs) #2,5,6,9,10

#model training-----
modelstring_ssvs_fitted <- "
model{

for(i in 1:N){
   
   logit(prob[i]) <- alpha + beta[1]*x[i,1] + beta[2]*x[i,2] + beta[3]*x[i,3] + 
      beta[4]*x[i,4] + beta[5]*x[i,5] 
   
   y[i] ~ dbern(prob[i])
}

alpha ~ dnorm(0,0.0001)

for(j in 1:5){
  beta[j] ~ dnorm(0,0.0001)
}
}
"

dir <- "Models"
modelfile_ssvs_fitted <- file.path(dir,"project_ssvs_fitted.txt")
writeLines(modelstring_ssvs_fitted,modelfile_ssvs_fitted)

dat_ssvs_fitted <- list(N=nrow(trainData),
                        x=trainData[,c(4,7,8,11,12)],
                        y=trainData$diagnosis)

jagsModel_ssvs_fitted <- jags.model(file=modelfile_ssvs_fitted, 
                                    data=dat_ssvs_fitted,
                                    n.chains = 2, n.adapt = 1000
)

# Use the function 'update' for burnin iterations
update(jagsModel_ssvs_fitted, n.iter=1000)

# Now run MCMC and collect posterior samples in coda format
codaSamples_ssvs_fitted <- coda.samples(
  model=jagsModel_ssvs_fitted, 
  variable.names = c("alpha","beta","gamma"), 
  n.iter = 10000, thin = 1)
summary(codaSamples_ssvs_fitted)


#model prediction----
beta.ssvs=t(c(1.9983,0.2716,72.6901,69.1219,4.2553,-371.7644))
beta.matrix.ssvs=matrix(beta.ssvs)
#for test data
X.vec.ssvs=c(rep(1,nrow(testData)),testData$texture.mean,
             testData$smoothness.mean,testData$compactness.mean,
             testData$symmetry.mean,testData$fractaldimension.mean)  
X.matrix.ssvs=matrix(X.vec.ssvs,byrow = FALSE,ncol = 6)
Z.ssvs=X.matrix.ssvs %*% beta.matrix.ssvs
P.ssvs=exp(Z.ssvs)/(1+exp(Z.ssvs))

#model evaluation-----
testData_ssvs = cbind(testData,P.ssvs)
#true negative
nrow(testData_ssvs[which(
  testData_ssvs$diagnosis==0&testData_ssvs$P.ssvs<0.5),])
#false positive
nrow(testData_ssvs[which(
  testData_ssvs$diagnosis==0&testData_ssvs$P.ssvs>=0.5),])
#false negative
nrow(testData_ssvs[which(
  testData_ssvs$diagnosis==1&testData_ssvs$P.ssvs<0.5),])
#true positive
nrow(testData_ssvs[which(
  testData_ssvs$diagnosis==1&testData_ssvs$P.ssvs>=0.5),]) 

# #for train data
# X.vec.ssvs.train=c(rep(1,nrow(trainData)),trainData$texture.mean,
#                    trainData$smoothness.mean,trainData$compactness.mean,
#                    trainData$symmetry.mean,trainData$fractaldimension.mean)  
# X.matrix.ssvs.train=matrix(X.vec.ssvs.train,byrow = FALSE,ncol = 6)
# Z.ssvs.train=X.matrix.ssvs.train %*% beta.matrix.ssvs
# P.ssvs.train=exp(Z.ssvs.train)/(1+exp(Z.ssvs.train))
# 
# #evaluation
# trainData_ssvs = cbind(trainData,P.ssvs.train)
# #true negative
# nrow(trainData_ssvs[which(
#   trainData_ssvs$diagnosis==0&trainData_ssvs$P.ssvs<0.5),])
# #false positive
# nrow(trainData_ssvs[which(
#   trainData_ssvs$diagnosis==0&trainData_ssvs$P.ssvs>=0.5),])
# #false negative
# nrow(trainData_ssvs[which(
#   trainData_ssvs$diagnosis==1&trainData_ssvs$P.ssvs<0.5),])
# #true positive
# nrow(trainData_ssvs[which(
#   trainData_ssvs$diagnosis==1&trainData_ssvs$P.ssvs>=0.5),]) 