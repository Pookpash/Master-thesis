require(CircStats)

setwd("C:/Users/Pook/Documents/Masterthesis")

#function to create data for a 2state HMM with 1 variable
simple2s1v <- function(n, s1to1prob, s2to2prob, mu, sigma, nanim){
  tpm <- matrix(c(s1to1prob,1-s1to1prob,1-s2to2prob,s2to2prob),ncol=2,byrow=T)
  stateseq <- createstateseq(n,tpm,nanim)
  data <- rnorm(n,mu[stateseq],sigma[stateseq])
  id <- sort(c(rep(seq(1,nanim,by=1),n)))
  data <- cbind(id,data,stateseq)
  data <- as.data.frame(data)
  colnames(data) <- c("id","varnorm","state")
  return(data)
}

#funtion to create data for a N-state HMM with 2 variables (norm+vonMis)
Ns2v <- function(n, tpm, mun, sigman, muvm, conc, nanim, N){
  stateseq <- createstateseq(n, tpm, nanim)
  datanorm <- rnorm(n, mun[stateseq], sigman[stateseq])
  
  datavonm <- rep(NA,n*nanim)
  for (i in 1:n*nanim){ #necessary bc rvm can not handle vectors
    datavonm[i] <- rvm(1,muvm[stateseq[i]],conc[stateseq[i]])
  }
  datavonm <- datavonm - pi
  
  id <- sort(c(rep(seq(1, nanim, by=1), n)))
  data <- cbind(id, datanorm, datavonm, stateseq)
  data <- as.data.frame(data)
  colnames(data) <- c("id", "varnorm", "varvm", "state")
  return(data)
}

create_tpm <- function(vec, N){
  tpm <- diag(N)*20 # *20 to make sure that it is most likely to stay in 
                    # current state when other elemts of tpm are drawn 
                    # from uniform(0,1)
  tpm[!tpm] <- exp(vec)
  tpm <- tpm/apply(tpm, 1, sum)
  return(tpm)
}

#create a stateseq
createstateseq <- function(n,tpm,nanim){ 
  N <- dim(tpm)[1]
  delta <- solve(t(diag(N)-tpm+1),c(rep(1,N)))
  s <- c()
  for(i in 1:nanim){
    k<- c(rep(NA,n))
    k[1] <- sample (1:N,size=1,prob=delta)
    for(t in 2:n)try({
      k[t] <- sample(1:N,size=1,prob=tpm[k[t-1],])
    })
    s <- c(s,k)
  }
  return(s)
}

###functions to create mutliple random datasets

create_parmat2s1v <- function(times, s11range, s22range, murange, sigrange){
  parmat <- matrix(0,ncol = 6,nrow = times)
  for(i in 1:times){
    parmat[i,1] <- runif(1,s11range[1],s11range[2])
    parmat[i,2] <- runif(1,s22range[1],s22range[2])
    parmat[i,3] <- runif(1,murange[1],murange[2])
    parmat[i,4] <- runif(1,murange[3],murange[4])
    parmat[i,5] <- runif(1,sigrange[1],sigrange[2])
    parmat[i,6] <- runif(1,sigrange[3],sigrange[4])
  }
  colnames(parmat) <- c("s11","s22","mu1","mu2","sig1","sig2")
  return(parmat)
}

create_parmatNs2v <- function(times, munmin, munmax, signmin, signmax, muvmmin, 
                              muvmmax, concmin, concmax, N){
  parvec <- c()
  for(i in 1:times){
    helpvec <- c(runif(N*(N-1)), runif(N, munmin, munmax), 
                 runif(N, signmin, signmax), runif(N, muvmmin, muvmmax), 
                 runif(N, concmin, concmax))
    parvec <- c(parvec,helpvec)
  }
  parmat <- matrix(parvec, ncol = N*N+3*N, nrow = times, byrow=T)
  return(parmat)
}


create_simset2s1v <- function(times, n, nanim, parmat){
  data2s1v <- list()
  for(i in 1:times){
    s11 <- parmat[i,1]
    s22 <- parmat[i,2]
    mu <-c(parmat[i,3:4])
    sigma <-c(parmat[i,5:6])
    data2s1v[[i]] <- simple2s1v(n,s11,s22,mu,sigma,nanim)
    print(paste0("Sucess for dataset ", i))
  }
  return(data2s1v)
}

create_simsetNs2v <- function(times, n, nanim, parmat, N){ #WIP
  dataNs2v <- list()
  for(i in 1:times){
    tpm    <- create_tpm(parmat[i, 1 : (N*(N-1))],N)
    mun    <- c(parmat[i, (N*(N-1)+1) : (N*N)])
    sigman <- c(parmat[i, (N*N+1)     : (N*(N+1))])
    muvm   <- c(parmat[i, (N*(N+1)+1) : (N*(N+2))])
    conc   <- c(parmat[i, (N*(N+2)+1) : (N*(N+3))])
    dataNs2v[[i]] <- Ns2v(n, tpm, mun, sigman, muvm, conc, nanim, N)
    print(paste0("Sucess for dataset ", i))
  }
  return(dataNs2v)
}

###creation of datasets

## 2state 1 variable
parmat2s1v <- create_parmat2s1v(1000, c(0.7,0.95), c(0.7,0.95), 
                                c(40,60,100,200), c(1,3,5,10))
data2s1v <- create_simset2s1v(1000, 100, 1, parmat2s1v)

## Nstate, 2 variables
parmat3s2v <- create_parmatNs2v(1, c(20,50,100), c(30,75,200), c(1,3,5), 
                                c(2,6,10), c(pi-.1,pi-.1,pi-.1), c(pi+.1,pi+.1,pi+.1), 
                                c(2,4,8), c(4,8,16), 3)

data3s2v <- create_simsetNs2v(1,10000,1,parmat3s2v,3)
