
set.seed(1)

trueb0 <- 0
trueb1 <- 2
trueSd <- 5
sampleSize <- 31

# create independent x-values 
x <- (-(sampleSize-1)/2):((sampleSize-1)/2)
# create dependent values according to ax + b + N(0,sd)
y <-  trueb0 + trueb1 * x + rnorm(n=sampleSize,mean=0,sd=trueSd)

plot(x,y, main="Test Data")

likelihood <- function(param){
  b0 = param[1]
  b1 = param[2]
  sd = param[3]
  
  pred = b0 + b1*x 
  singlelikelihoods = dnorm(y, mean = pred, sd = sd, log = T)
  sumll = sum(singlelikelihoods)
  return(sumll)   
}

#specify prior distributions
prior <- function(param){
  b0 = param[1]
  b1 = param[2]
  sd = param[3]
  b0prior = dnorm(b0, sd = 5, log = T)
  b1prior = dnorm(b1, sd = 5, log = T)
  sdprior = dunif(sd, min=0, max=30, log = T)
  return(b0prior+b1prior+sdprior)
}

#non normalized posterior
posterior <- function(param){
  return (likelihood(param) + prior(param))
}

proposalfunction <- function(param){
  return(rnorm(3,mean = param, sd= c(0.5,0.1,0.3)))
}

run_metropolis_MCMC <- function(startvalue, iterations, plot = FALSE){
  par(mfrow = c(1,2))
  chain = array(dim = c(iterations+1,3))
  chain[1,] = startvalue
  for (i in 1:iterations){
    proposal = proposalfunction(chain[i,])
    
    probab = exp(posterior(proposal) - posterior(chain[i,]))
    if (runif(1) < probab){
      chain[i+1,] = proposal
    }else{
      chain[i+1,] = chain[i,]
    }
    # flush.console()
    # hist(chain[1:i+1,2],nclass=30, main="Posterior of b1", xlab="True value = blue line", col= "lightgreen")
    # abline(v = mean(chain[1:i+1,2]) ,lwd = 2)
    # abline(v = trueb1, col="blue" ,lwd = 2)
    # plot(chain[1:i+1,2], type = "l", xlab="True value = blue line" , main = "Chain values of b1", lwd = 2)
    # abline(h = trueb1, col="blue" , lwd = 2)
    # Sys.sleep(.09)
  }
  return(chain)
}

startvalue = c(1,1.4,6)
chain = run_metropolis_MCMC(startvalue, 250)

#Till, comment out the updating plots before running this!
startvalue = c(1,1.4,6)
chain = run_metropolis_MCMC(startvalue, 10000)
burnIn = 5000

### Summary: ###

par(mfrow = c(2,3))
hist(chain[-(1:burnIn),1],nclass=30, main="Posterior of b0", xlab="True value = blue line", col = "lightgreen")
abline(v = mean(chain[-(1:burnIn),1]), lwd = 2)
abline(v = trueb0, col="blue", lwd = 2)
hist(chain[-(1:burnIn),2],nclass=30, main="Posterior of b1", xlab="True value = blue line", col = "lightgreen")
abline(v = mean(chain[-(1:burnIn),2]), lwd = 2)
abline(v = trueb1, col="blue", lwd = 2)
hist(chain[-(1:burnIn),3],nclass=30, main="Posterior of sd", xlab="True value = blue line", col = "lightgreen")
abline(v = mean(chain[-(1:burnIn),3]), lwd = 2)
abline(v = trueSd, col="blue", lwd = 2)
plot(chain[-(1:burnIn),1], type = "l", xlab="True value = blue line" , main = "Chain values of b0")
abline(h = trueb0, col="blue", lwd = 2)
plot(chain[-(1:burnIn),2], type = "l", xlab="True value = blue line" , main = "Chain values of b1")
abline(h = trueb1, col="blue", lwd = 2)
plot(chain[-(1:burnIn),3], type = "l", xlab="True value = blue line" , main = "Chain values of sd")
abline(h = trueSd, col="blue", lwd = 2)


# for comparison:

summary(lm(y~x))

