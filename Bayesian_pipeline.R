library(ggplot2)
library(ggrepel)
library(gridExtra)
library(grid)
library(reshape2)
library(reshape)
library(dplyr)
library(factoextra)

#read data and data preprocessing------------  
data <- read.csv("data", header=FALSE)
colnames(data) = c('ID','diagnosis',
                   'radius.mean','texture.mean','perimeter.mean',
                   'area.mean','smoothness.mean','compactness.mean',
                   'concavity.mean','concavepoints.mean','symmetry.mean',
                   'fractaldimension.mean', 'radius.se','texture.se',
                   'perimeter.se','area.se','smoothness.se','compactness.se',
                   'concavity.se','concavepoints.se','symmetry.se',
                   'fractaldimension.se','radius.wrost','texture.wrost',
                   'perimeter.wrost','area.wrost','smoothness.wrost',
                   'compactness.wrost','concavity.wrost','concavepoints.wrost',
                   'symmetry.wrost','fractaldimension.wrost')
data$diagnosis = as.character(data$diagnosis)
data$diagnosis[which(data$diagnosis=='B')] = 0
data$diagnosis[which(data$diagnosis=='M')] = 1
data$diagnosis = as.numeric(data$diagnosis)


trainData.index=sample(1:nrow(data),size=floor(3/4*nrow(data)),replace = FALSE)
trainData=data[trainData.index,]
testData=data[-trainData.index,]

#descriptive analysis---------------
data.summary = data.frame(diagnosis=NULL,variable = NULL, rep(NULL,6))
for (i in 3:32) {
   data.summary = rbind(
      data.summary,
      data.frame(
         diagnosis='M',
         variable = colnames(data)[i],
         t(data.frame(unclass(summary(data[which(data$diagnosis==1),i]))))))
   data.summary = rbind(
      data.summary,
      data.frame(
         diagnosis='B',
         variable = colnames(data)[i],
         t(data.frame(unclass(summary(data[which(data$diagnosis==0),i]))))))
}

rownames(data.summary) = NULL

ggplot(data.summary[1:10,],aes(x = Mean, y = variable,color = diagnosis)) +
   geom_point(size = 1.5,position = position_dodge(width = 1)) +
   geom_errorbar(aes(xmax = Max., xmin = Min.),
                 position = position_dodge(width = 1),width=0.5, size=0.8) +
   theme(panel.grid.major.y = element_blank(),
         panel.grid.minor.y = element_blank())

###plot selected features
dat.m <- reshape2::melt(data[,1:12],id.vars='diagnosis', 
                        measure.vars=c('radius.mean','texture.mean',
                                       'perimeter.mean','area.mean',
                                       'smoothness.mean','compactness.mean',
                                       'concavity.mean','concavepoints.mean',
                                       'symmetry.mean','fractaldimension.mean'))
dat.m$diagnosis = factor(dat.m$diagnosis   , levels=c('0','1'))
dat.m$diagnosis = recode(dat.m$diagnosis , '0' = "B", '1' = "M")

ggplot(data = dat.m) + geom_boxplot(aes(x=variable, y=value,fill=diagnosis))

p1 = ggplot(dat.m[which(dat.m$variable=='radius.mean'),], 
            aes(x = diagnosis, y = value)) +           
   geom_boxplot() + xlab("") + ylab('radius')

p2 = ggplot(dat.m[which(dat.m$variable=='texture.mean'),], 
            aes(x = diagnosis, y = value)) +            
   geom_boxplot() + xlab("") + ylab('texture')

p3 = ggplot(dat.m[which(dat.m$variable=='perimeter.mean'),], 
            aes(x = diagnosis, y = value)) +            
   geom_boxplot() + xlab("") + ylab('perimeter')

p4 = ggplot(dat.m[which(dat.m$variable=='area.mean'),], 
            aes(x = diagnosis, y = value)) +            
   geom_boxplot() + xlab("") + ylab('area')

p5 = ggplot(dat.m[which(dat.m$variable=='smoothness.mean'),], 
            aes(x = diagnosis, y = value)) +            
   geom_boxplot() + xlab("") + ylab('smoothness')

p6 = ggplot(dat.m[which(dat.m$variable=='compactness.mean'),], 
            aes(x = diagnosis, y = value)) +            
   geom_boxplot() + xlab("") + ylab('compactness')

p7 = ggplot(dat.m[which(dat.m$variable=='concavity.mean'),], 
            aes(x = diagnosis, y = value)) +            
   geom_boxplot() + xlab("") + ylab('concavity')

p8 = ggplot(dat.m[which(dat.m$variable=='concavepoints.mean'),],
            aes(x = diagnosis, y = value)) +            
   geom_boxplot() + xlab("") + ylab('concave points')

p9 = ggplot(dat.m[which(dat.m$variable=='symmetry.mean'),], 
            aes(x = diagnosis, y = value)) +            
   geom_boxplot() + xlab("") + ylab('symmetry')

p10 = ggplot(dat.m[which(dat.m$variable=='fractaldimension.mean'),], 
             aes(x = diagnosis, y = value)) +            
   geom_boxplot() + xlab("") + ylab('fractal dimension')

grid.arrange(p1, p2,p3,p4,p5,p6,p7,p8,p9,p10, nrow = 2)

#modeling-----------------
###Bayesian ssvs------
source("Bayesian ssvs.R")

###Bayesian lasso-----
source("Bayesian lasso.R")

### Frequentist PCA-----
source("Frequentist PCA.R")