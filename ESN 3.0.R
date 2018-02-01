setwd("~/Dropbox/Research with Qiuyi Wu/dataset/EEG_shuffled")
library(glmnet)
source('ESN function.R')
data <- read.csv("A&E.csv")

# generate the ESN reservoir
# leaking rate 0.3
# trainLen = testLen = 2000, initLen = 100
# inSize = outSize = 1, resSize = 300
res(0.3)

#######################################################################



set.seed(42)
# Unifrom Dist
unif(0.5)
#arcs(0.5)
#arcs(1)
#norm(1/6)
#norm(sqrt(1/12))



#######################################################################
# Run the reservoir, we get X, X_T and label Yt
ESN()



# get output weight
A <- glmnet(X_T, Yt, alpha=0,family = 'binomial')
plot(A,xvar="lambda",label=TRUE)

#Get Wout
glmnet_cv <- cv.glmnet(X_T, Yt)
mod <- glmnet(X_T, Yt,alpha=0, family = 'binomial',lambda = glmnet_cv$lambda.min)
summary(mod)
Wout <- mod$beta

###############################################################

#Run the trained ESN 
test()




Y <- as.factor(predict(mod, X_test_T, type='class'))
Y_ori <- Ytotal[(trainLen+1):(trainLen+testLen)]
table(Y, Y_ori )

(error <- length(which(Yt != Y))/length(Y))
(error_test <- length(which(Y != Y_ori))/length(Y))


########################################################################


data <- data[,-1]
Ytotal = data[,179]
Xtotal = data[,1:178]
Ytotal <-as.matrix(Ytotal)
cv <- cv.glmnet(as.matrix(Xtotal[1:2000,]), Ytotal[1:2000], alpha=1, family="binomial")
mod2 <- glmnet(as.matrix(Xtotal[1:2000,]), Ytotal[1:2000], alpha=1,family="binomial", lambda = cv$lambda.min)
Y2<-as.factor(predict(mod2, as.matrix(Xtotal[1:2000,]), type = 'class'))

(error_train <- length(which(Y2 != Ytotal[1:2000]))/length(Y2))





####################### raindomForest ####################################
library(randomForest)
mod3 <- randomForest(Xtotal[1:2000,], as.factor(Ytotal[1:2000]), nbtree = 500)
Y3<-as.factor(predict(mod3, Xtotal[1:2000,], type = 'class'))

(error_train <- length(which(Y3 != Ytotal[1:2000]))/length(Y3))

Y3t<-as.factor(predict(mod3, Xtotal[2001:4000,], type = 'class'))
(error_test <- length(which(Y3t != Ytotal[2001:4000]))/length(Y3t))















