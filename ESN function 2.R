#%%%%%%%%%%%%% function for reservoir size %%%%%%%%%%%%%

res <- function(r)
{
  data = data[,-1]
  Ytotal = data[,179]
  Xtotal = data[,1:178]
  # load the data
  a = r  # leaking rate
  trainLen = 2000
  testLen = 2000
  initLen = 100
  # reservoir size
  inSize = outSize = 1
  resSize = 50
  newList <- list("Ytotal" = Ytotal, "Xtotal" = Xtotal,
                  "trainLen" = trainLen, "testLen" = testLen,
                  "initLen" = initLen, "resSize" = resSize,
                  "inSize" = inSize, "outSize" = outSize, "a" = a)
  list2env(newList ,.GlobalEnv)
}

#%%%%%%%%%%%%% function for reservoir size %%%%%%%%%%%%%







#%%%%%%%%%%%%% function for distribution of the weights %%%%%%%%%%%%%

arcs <- function(r)  # r= 0.5 and 1
{
  Uin <- matrix(runif(resSize*(1+inSize)),resSize)
  U <- matrix(runif(resSize*resSize),resSize)
  Win <- -r + 2*r*(sin(pi*Uin/2))^2
  W <- -r +  2*r*(sin(pi*U/2))^2
  # Nnormalizing and setting spectral radius:
  rhoW = abs(eigen(W,only.values=TRUE)$values[1])
  W = W * 1.25 / rhoW
  
  newList <- list("Win" = Win, "W" = W)
  list2env(newList ,.GlobalEnv)
}


unif <- function(r)  # r = 0.5
{
  Win = matrix(runif(resSize*(1+inSize),-r,r),resSize)
  W = matrix(runif(resSize*resSize,-r, r),resSize)
  rhoW = abs(eigen(W,only.values=TRUE)$values[1])
  W = W * 1.25 / rhoW
  
  newList <- list("Win" = Win, "W" = W)
  list2env(newList ,.GlobalEnv)
}


norm <- function(r)   # r = 1/6  and  sqrt(1/12)
{
  Win = matrix(rnorm(resSize*(1+inSize),0,r),resSize)
  W = matrix(rnorm(resSize*resSize,0,r),resSize)
  iz <- sample(1:(resSize*resSize), size = resSize*resSize*0.7)
  W[iz] <- 0
  rhoW = abs(eigen(W,only.values=TRUE)$values[1])
  W = W * 1.25 / rhoW
  
  newList <- list("Win" = Win, "W" = W)
  list2env(newList ,.GlobalEnv)
}

#%%%%%%%%%%%%% function for distribution of the weights %%%%%%%%%%%%%









#%%%%%%%%%%%%% function for run the reservoir %%%%%%%%%%%%%

# allocated memory for the design (collected states) matrix
ESN <- function()
{
  X = matrix(0,resSize,trainLen-initLen)
  # set the corresponding target matrix directly
  Yt = matrix(Ytotal[(initLen+2):(trainLen+1)],1)
  
  # run the reservoir with the data and collect X
  x = rep(0,resSize)
  for (t in 1:trainLen){
    for (s in 1: 178) {
      u = Xtotal[t,s]
      x = (1-a)*x + a*tanh( Win %*% rbind(1,u) + W %*% x )
    }
    if (t > initLen)
      X[,t-initLen] = x
  }
  
  X_T = t(X)
  
  newList <- list("X" = X, "Yt" = Yt, "X_T" = X_T)
  list2env(newList ,.GlobalEnv)
}


#%%%%%%%%%%%%% function for run the reservoir %%%%%%%%%%%%%






#%%%%%%%%%%%%% Test -Run the trained ESN  %%%%%%%%%%%%%

#Run the trained ESN 
test <- function()
{
  X_test = matrix(0,resSize,testLen)
  x = rep(0,resSize)
  for (t in 2001:(2000+testLen)){
    for (s in 1: 178) {
      u = Xtotal[t,s]
      x = (1-a)*x + a*tanh( Win %*% rbind(1,u) + W %*% x )
    }
    X_test[,t-2000] = x
  }
  X_test_T = t(X_test)
  
  newList <- list("X_test" = X_test, "X_test_T" = X_test_T)
  list2env(newList ,.GlobalEnv)
}
