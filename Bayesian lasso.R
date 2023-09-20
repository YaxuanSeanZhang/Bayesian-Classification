#variable selection-----
modelstring_lasso <- "
model{

for(i in 1:N){
   
   logit(prob[i]) <- alpha + beta[1]*x[i,1] + beta[2]*x[i,2] + beta[3]*x[i,3] + 
      beta[4]*x[i,4] + beta[5]*x[i,5] + beta[6]*x[i,6] + beta[7]*x[i,7] + 
      beta[8]*x[i,8] + beta[9]*x[i,9] + beta[10]*x[i,10]    
      
    y[i] ~ dbern(prob[i])
}

alpha ~ dnorm(0,0.0001)

for (j in 1:10) {
  beta[j] ~ ddexp(0,sigma/lambda)
}

sigma~dgamma(0.01,0.01)
lambda ~ dt(mu,tau,1)T(0,)
mu~dnorm(0,0.0001)
tau~dgamma(0.01,0.01)
}
"

dir <- "Models"
modelfile_lasso <- file.path(dir,"project_lasso.txt")
writeLines(modelstring_lasso,modelfile_lasso)

dat_lasso <- list(N=nrow(data),
                  x=data[,3:12],
                  y=data$diagnosis)

jagsModel_lasso <- jags.model(file=modelfile_lasso, 
                              data=dat_lasso,
                              n.chains = 2, n.adapt = 1000
)

# Use the function 'update' for burnin iterations
update(jagsModel_lasso, n.iter=1000)

# Now run MCMC and collect posterior samples in coda format
codaSamples_lasso <- coda.samples(model=jagsModel_lasso, 
                                  variable.names = c("alpha","beta"), 
                                  n.iter = 10000, thin = 1)
summary(codaSamples_lasso)  #2,4,5,8

#visualization for Posterior Distributions and 95% Credible Intervals 
p1 = ggplot(data = data.frame(x = c(-5, 2) ), aes(x)) + 
  stat_function(fun = dnorm, n = 1000, 
                args = list(mean = -1.55351, sd = 0.742213),
                geom = 'area', fill = 'lightblue') + 
  stat_function(fun = dnorm, n = 1000, 
                args = list(mean = -1.55351, sd = 0.742213),geom = 'line') + 
  ggtitle("radius") + scale_y_continuous(breaks = NULL) + 
  geom_vline(xintercept =-2.74677,linetype="dotted") + 
  geom_vline(xintercept =-0.32413,linetype="dotted") +
  xlab('beta') + ylab('') + geom_vline(xintercept =0,color = 'red') + 
  theme(plot.title = element_text(hjust = 0.5))

p2 = ggplot(data = data.frame(x = c(-0.01, 0.75) ), aes(x)) + 
  stat_function(fun = dnorm, n = 1000, 
                args = list(mean = 0.35974, sd = 0.063708),
                geom = 'area', fill = 'lightblue') + 
  stat_function(fun = dnorm, n = 1000, 
                args = list(mean = 0.35974, sd = 0.063708),geom = 'line') + 
  ggtitle("texture") + scale_y_continuous(breaks = NULL) + 
  geom_vline(xintercept =0.24277,linetype="dotted") + 
  geom_vline(xintercept =0.49073,linetype="dotted") +
  xlab('beta') + ylab('') + geom_vline(xintercept =0,color = 'red') + 
  theme(plot.title = element_text(hjust = 0.5))

p3 = ggplot(data = data.frame(x = c(-0.5, 0.3) ), aes(x)) + 
  stat_function(fun = dnorm, n = 1000, 
                args = list(mean = -0.07699, sd = 0.091135),
                geom = 'area', fill = 'lightblue') + 
  stat_function(fun = dnorm, n = 1000, 
                args = list(mean = -0.07699, sd = 0.091135),geom = 'line') + 
  ggtitle("perimeter") + scale_y_continuous(breaks = NULL) + 
  geom_vline(xintercept =-0.25191,linetype="dotted") + 
  geom_vline(xintercept =0.09047,linetype="dotted") +
  xlab('beta') + ylab('') + geom_vline(xintercept =0,color = 'red') + 
  theme(plot.title = element_text(hjust = 0.5))

p4 = ggplot(data = data.frame(x = c(-0.02, 0.08) ), aes(x)) + 
  stat_function(fun = dnorm, n = 1000, 
                args = list(mean = 0.03514, sd = 0.009417),
                geom = 'area', fill = 'lightblue') + 
  stat_function(fun = dnorm, n = 1000, 
                args = list(mean = 0.03514, sd = 0.009417),geom = 'line') + 
  ggtitle("area") + scale_y_continuous(breaks = NULL) + 
  geom_vline(xintercept =0.01564,linetype="dotted") + 
  geom_vline(xintercept =0.05324,linetype="dotted") +
  xlab('beta') + ylab('') + geom_vline(xintercept =0,color = 'red') + 
  theme(plot.title = element_text(hjust = 0.5))

p5 = ggplot(data = data.frame(x = c(-75, 175) ), aes(x)) + 
  stat_function(fun = dnorm, n = 1000, 
                args = list(mean = 46.76889, sd = 27.160858),
                geom = 'area', fill = 'lightblue') + 
  stat_function(fun = dnorm, n = 1000, 
                args = list(mean = 46.76889, sd = 27.160858),geom = 'line') + 
  ggtitle("smoothness") + scale_y_continuous(breaks = NULL) + 
  geom_vline(xintercept =3.37346,linetype="dotted") + 
  geom_vline(xintercept =109.03057,linetype="dotted") +
  xlab('beta') + ylab('') + geom_vline(xintercept =0,color = 'red') + 
  theme(plot.title = element_text(hjust = 0.5))

p6 = ggplot(data = data.frame(x = c(-40, 40) ), aes(x)) + 
  stat_function(fun = dnorm, n = 1000, 
                args = list(mean = -1.37867, sd = 8.526762),
                geom = 'area', fill = 'lightblue') + 
  stat_function(fun = dnorm, n = 1000, 
                args = list(mean = -1.37867, sd = 8.526762),geom = 'line') + 
  ggtitle("compactness") + scale_y_continuous(breaks = NULL) + 
  geom_vline(xintercept =-20.08327,linetype="dotted") + 
  geom_vline(xintercept =15.68460,linetype="dotted") +
  xlab('beta') + ylab('') + geom_vline(xintercept =0,color = 'red') + 
  theme(plot.title = element_text(hjust = 0.5))

p7 = ggplot(data = data.frame(x = c(-25, 35) ), aes(x)) + 
  stat_function(fun = dnorm, n = 1000, 
                args = list(mean = 5.57421, sd = 6.338129),
                geom = 'area', fill = 'lightblue') + 
  stat_function(fun = dnorm, n = 1000, 
                args = list(mean = 5.57421, sd = 6.338129),geom = 'line') + 
  ggtitle("concavity") + scale_y_continuous(breaks = NULL) + 
  geom_vline(xintercept =-5.79261,linetype="dotted") + 
  geom_vline(xintercept =19.09447,linetype="dotted") +
  xlab('beta') + ylab('') + geom_vline(xintercept =0,color = 'red') + 
  theme(plot.title = element_text(hjust = 0.5))

p8 = ggplot(data = data.frame(x = c(-50, 175) ), aes(x)) + 
  stat_function(fun = dnorm, n = 1000, 
                args = list(mean = 64.62679, sd = 25.143904),
                geom = 'area', fill = 'lightblue') + 
  stat_function(fun = dnorm, n = 1000, 
                args = list(mean = 64.62679, sd = 25.143904),geom = 'line') + 
  ggtitle("concave points") + scale_y_continuous(breaks = NULL) + 
  geom_vline(xintercept =16.66645,linetype="dotted") + 
  geom_vline(xintercept =114.63978,linetype="dotted") +
  xlab('beta') + ylab('') + geom_vline(xintercept =0,color = 'red') + 
  theme(plot.title = element_text(hjust = 0.5))

p9 = ggplot(data = data.frame(x = c(-25, 50) ), aes(x)) + 
  stat_function(fun = dnorm, n = 1000, 
                args = list(mean = 13.93246, sd = 9.279321),
                geom = 'area', fill = 'lightblue') + 
  stat_function(fun = dnorm, n = 1000, 
                args = list(mean = 13.93246, sd = 9.279321),geom = 'line') + 
  ggtitle("symmetry") + scale_y_continuous(breaks = NULL) + 
  geom_vline(xintercept =-2.93755,linetype="dotted") + 
  geom_vline(xintercept =32.43180,linetype="dotted") +
  xlab('beta') + ylab('') + geom_vline(xintercept =0,color = 'red') + 
  theme(plot.title = element_text(hjust = 0.5))

p10 = ggplot(data = data.frame(x = c(-100, 100) ), aes(x)) + 
  stat_function(fun = dnorm, n = 1000, 
                args = list(mean = -2.57383, sd = 24.002242),
                geom = 'area', fill = 'lightblue') + 
  stat_function(fun = dnorm, n = 1000, 
                args = list(mean = -2.57383, sd = 24.002242),geom = 'line') + 
  ggtitle("fractal dimension") + scale_y_continuous(breaks = NULL) + 
  geom_vline(xintercept =-52.42050,linetype="dotted") + 
  geom_vline(xintercept =52.42821,linetype="dotted") +
  xlab('beta') + ylab('') + geom_vline(xintercept =0,color = 'red') + 
  theme(plot.title = element_text(hjust = 0.5))

grid.arrange(p1, p2,p3,p4,p5,p6,p7,p8,p9,p10, nrow = 2)

grid.arrange(p1, p2,p4,p5,p8, nrow = 1)

#model training-----
modelstring_lasso_fitted <- "
model{

for(i in 1:N){
   
   logit(prob[i]) <- alpha + beta[1]*x[i,1] + beta[2]*x[i,2] + 
      beta[3]*x[i,3] + beta[4]*x[i,4] + beta[5]*x[i,5]
    y[i] ~ dbern(prob[i])
}

alpha ~ dnorm(0,0.0001)

for (j in 1:5) {
  beta[j] ~ dnorm(0,0.0001)
}
}
"

dir <- "Models"
modelfile_lasso_fitted <- file.path(dir,"project_lasso_fitted.txt")
writeLines(modelstring_lasso_fitted,modelfile_lasso_fitted)

dat_lasso_fitted <- list(N=nrow(trainData),
                         x=trainData[,c(3,4,6,7,10)],
                         y=trainData$diagnosis)

jagsModel_lasso_fitted <- jags.model(file=modelfile_lasso_fitted, 
                                     data=dat_lasso_fitted,
                                     n.chains = 2, n.adapt = 1000
)

# Use the function 'update' for burnin iterations
update(jagsModel_lasso_fitted, n.iter=1000)

# Now run MCMC and collect posterior samples in coda format
codaSamples_lasso_fitted <- coda.samples(model=jagsModel_lasso_fitted, 
                                         variable.names = c("alpha","beta"), 
                                         n.iter = 10000, thin = 1)
summary(codaSamples_lasso_fitted) 

#model prediction----
beta.lasso=t(c(-15.80628,-1.47923,-1.47923,0.02775,71.99721,80.71344))
beta.matrix.lasso=matrix(beta.lasso)

#for test data
X.vec.lasso=c(rep(1,nrow(testData)),testData$radius.mean,
              testData$texture.mean,testData$area.mean,
              testData$smoothness.mean,testData$concavepoints.mean)  
X.matrix.lasso=matrix(X.vec.lasso,byrow = FALSE,ncol = 6)
Z.lasso=X.matrix.lasso %*% beta.matrix.lasso
P.lasso=exp(Z.lasso)/(1+exp(Z.lasso))

#model evaluation-----
testData_lasso = cbind(testData,P.lasso)
#true negative
nrow(testData_lasso[which(
  testData_lasso$diagnosis==0&testData_lasso$P.lasso<0.5),])
#false positive
nrow(testData_lasso[which(
  testData_lasso$diagnosis==0&testData_lasso$P.lasso>=0.5),])
#false negative
nrow(testData_lasso[which(
  testData_lasso$diagnosis==1&testData_lasso$P.lasso<0.5),])
#true positive
nrow(testData_lasso[which(
  testData_lasso$diagnosis==1&testData_lasso$P.lasso>=0.5),]) 

# #for train data
# X.vec.lasso.train=c(rep(1,nrow(trainData)),trainData$texture.mean,
#                     trainData$area.mean,trainData$smoothness.mean,
#                     rainData$concavepoints.mean)  
# X.matrix.lasso.train=matrix(X.vec.lasso.train,byrow = FALSE,ncol = 5)
# Z.lasso.train=X.matrix.lasso.train %*% beta.matrix.lasso
# P.lasso.train=exp(Z.lasso.train)/(1+exp(Z.lasso.train))
# 
# trainData_lasso = cbind(trainData,P.lasso.train)
# #true negative
# nrow(trainData_lasso[which(
#   trainData_lasso$diagnosis==0&trainData_lasso$P.lasso.train<0.5),])
# #false positive
# nrow(trainData_lasso[which(
#   trainData_lasso$diagnosis==0&trainData_lasso$P.lasso.train>=0.5),]) 
# #false negative
# nrow(trainData_lasso[which(
#   trainData_lasso$diagnosis==1&trainData_lasso$P.lasso.train<0.5),])  
# #true positive
# nrow(trainData_lasso[which(
#   trainData_lasso$diagnosis==1&trainData_lasso$P.lasso.train>=0.5),]) 