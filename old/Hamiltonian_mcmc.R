library(MASS)

#posterior <- normal distribution with mu=0 and sd=1
x1<-seq(-3,3,by=0.05)
x2<-dnorm(x1,0,1)
plot(x1,x2,type="l",lwd=3,col="lightblue")
nxlog <- -log(x2)
plot(x1,nxlog,type="l",lwd=3,col="lightblue")

pointx <- 0.8
npx <- -log(dnorm(pointx,0,1))
plot(x1,nxlog,type="l",lwd=4,col="lightblue")
points(pointx,npx,pch=19,lwd=8)


#Hamiltonian MCMC
x <- 0.8
delta <-0.3
nsamples <- 1
L=40

#Potentional Energy
U <- function(x){
        val <- (x-0)^2/(2*1^2)
        return(val)
}

#derivative of U
dU <- function(x){
        val <- x
        return(val)
}

#kinetic energy function
K <- function(p){
        val <- sum((t(p)*p))/2
        return(val)
}

t=0
while(t<nsamples){
       t<- t+1
       
       #sample random momentum
       p0 <-  2  #rnorm(1,0,1)
       
       ##Simulate Hamiltonian Dynamics
       #First 1/2 Step of Momentum
       pStar = p0 - delta/2*dU(x)
       
       # First full step for position/sample
       xStar = x + delta*pStar;
       
       # Full stepls
       for (i in 1:L-1){
               # Momentum
               pStar = pStar - delta*dU(xStar)
               # Position/sample
               xStar = xStar + delta*pStar
               flush.console()
               plot(x1,nxlog,type="l",lwd=4,col="lightblue")
               points(xStar,-log(dnorm(xStar,0,1)),pch=19,lwd=6)
               Sys.sleep(.05)
               
       }
       # Last half step
       pStar = pStar - delta/2*dU(xStar)
       
       # Evaluate Energies at start and end of trajectory
       U0 = U(x);
       UStar = U(xStar)
       
       K0 = K(p0);
       KStar = K(pStar)
       
       # Acceptance/rejection criterion
       alpha = min(1,exp((U0 + K0) - (UStar + KStar)))
       
       u = runif(1,0,1)
       
       if (u < alpha){
               x=xStar 
       }else{
               x = x
               
       }
}
x



### HMC multiple times
HMC <- function(xstart=0.8,delta=0.3,nsamples=10000,L=50,plot=FALSE){
        x <- c(rep(0,nsamples))
        x[1] <- xstart
        t=1
        while(t<nsamples){
                t<- t+1
                
                #sample random momentum
                p0 <- rnorm(1,0,1)
                
                ##Simulate Hamiltonian Dynamics
                #First 1/2 Step of Momentum
                pStar = p0 - delta/2*dU(x[t-1])
                
                # First full step for position/sample
                xStar = x[t-1] + delta*pStar;
                
                # Full stepls
                for (i in 1:L-1){
                        # Momentum
                        pStar = pStar - delta*dU(xStar)
                        # Position/sample
                        xStar = xStar + delta*pStar
                }
                # Last half step
                pStar = pStar - delta/2*dU(xStar)
                
                # Evaluate Energies at start and end of trajectory
                U0 = U(x[t-1]);
                UStar = U(xStar)
                
                K0 = K(p0);
                KStar = K(pStar)
                
                # Acceptance/rejection criterion
                alpha = min(1,exp((U0 + K0) - (UStar + KStar)))
                
                u = runif(1,0,1)
                
                if (u < alpha){
                        x[t]=xStar 
                }else{
                        x[t] = x[t-1]
                        
                }
                if (plot==T){
                        flush.console()
                        hist(x[1:t],nclass=15, main="Sampled posterior", xlab="True value = blue line", col= "lightgreen")
                        Sys.sleep(.06)
                }
        }
        return(x)
}

set.seed(1337)
chain <- HMC(nsamples=250,plot=T)

set.seed(1337)
chain<-HMC(nsamples=10000)
burnin <- 500

hist(chain[-(1:burnin)],nclass=15)

plot(density(chain[-(1:burnin)]))
points(x1,x2)


#HMC in higher dimensions

#Potentional Energy
U <- function(x){
        val <- t(x)%*%solve(matrix(c(1,.8,.8,1),nrow=2,byrow=T))%*%x
        return(val)
}

#derivative of U
dU <- function(x){
        val <- t(x)%*%solve(matrix(c(1,.8,.8,1),nrow=2,byrow=T))
        return(val)
}

#kinetic energy function
K <- function(p){
        val <- sum(t(p)%*%p)/2
        return(val)
}

HMCMV <- function(delta=0.3,nsamples=1000,L=50,plot=FALSE){
        par(mfrow=c(1,2))
        x <- matrix(c(rep(0,(nsamples*2))),nrow=2)
        x[,1] <- c(0,6) 
        t=1
        while(t<nsamples){
                t<- t+1
                
                #sample random momentum
                p0 <- rnorm(2,0,1)
                
                ##Simulate Hamiltonian Dynamics
                #First 1/2 Step of Momentum
                pStar = p0 - delta/2*dU(x[,t-1])
                
                # First full step for position/sample
                xStar = x[,t-1] + delta*pStar
                
                # Full stepls
                for (i in 1:L-1){
                        # Momentum
                        pStar = pStar - delta*dU(t(xStar))
                        # Position/sample
                        xStar = xStar + delta*pStar
                }
                # Last half step
                pStar = pStar - delta/2*dU(t(xStar))
                
                # Evaluate Energies at start and end of trajectory
                U0 = U(x[,t-1])
                UStar = U(t(xStar))
                
                K0 = K(p0)
                KStar = K(pStar)
                
                # Acceptance/rejection criterion
                alpha = min(1,exp((U0 + K0) - (UStar + KStar)))
                
                u = runif(1,0,1)
                
                if (u < alpha){
                        x[,t]=xStar 
                }else{
                        x[,t] = x[,t-1]
                        
                }
                if (plot==T){
                        if(t%%10==0||t<20){
                                bivn.kde <- kde2d(x[1,1:t], x[2,1:t], n = 50)
                                flush.console()
                                persp(bivn.kde, phi = 45, theta = 30, shade = .1, border = NA)
                                image(bivn.kde); contour(bivn.kde, add = T)
                                Sys.sleep(.06)
                        }
                }
        }
        return(x)
}

chainMV <- HMCMV(nsamples=1000,plot=T)

chainMV <- HMCMV(nsamples=5000)
par(mfrow=c(1,1))
bivn.kde <- kde2d(chainMV[1,], chainMV[2,], n = 50)
persp(bivn.kde, phi = 45, theta = 30, shade = .1, border = NA)
