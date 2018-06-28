

### @param I is an input vector of leaf litter
### @param X is an initial state

decomp_matrix <- function(I,X,fdat,fc,dry){
  #### Allocation
  B <- c(rep(1,16),0)

  #### Decay Rates
  K <- diag(17)
  #### Transfer
  A <- diag(17) * -1

  aet <- 600 #fix for now but will eventually go into E (environment) vector
  tyll = sum(I[1:16])
  ccll = 1.54 + .0457 * (fc - dry) #ccll = 1?
  if(tyll > ccll) tyll = ccll
  decmlt = 1 + (-.5+.075*(fc-dry))*(1-tyll/ccll)

  for(i in 1:16){

    if(I[i]==0 & X[i]==0) next()

    pwtlos <- (.9804 + .09352 * aet) - ((-.4956 + .00193 * aet) * (fdat[i,7] / fdat[i,2]))
    pwtlos <- (decmlt*pwtlos)/100
    K[i,i] <- pwtlos

    #calculate actual wt loss (t/ha)
    wtloss <- pwtlos*X[i]

    #calculate fraction of organic matter remaining
    pomr <- (I[i] * fdat[i,10]-wtloss)/(I[i] * fdat[i,10])

    #retain cohort for another year of decay if fraction remaining is greater
    #than fraction which will become humus of well decayed wood
    if(pomr <= (fdat[i,7] * 1.7039 + .0955)){
      wtloss <- (fdat[i,7] * 1.7039 + .0955)*(fdat[i,10])
      #transfer to humus
      A[17,i] <- wtloss
    }

  }

  #### Full Equation
  change_X <- B * I - A %*% K %*% X
  return(change_X)

}

#### Original Subroutine
source('decomp.R')
orig <- decomp(fdat, aet, ncohrt, fc, dry, tyl, C.mat)

#### Initial State
X <- ff[1:17,2]
#### Input
I <- c(tyl[1:16],0)

#### Matrix Math
mat_out <- decomp_matrix(I=I, X=X, fdat=fdat, fc=fc, dry=dry)

#### Plot Check
plot(orig$ff[1:17,2], ff[1:17,2] + mat_out, pch=19)
abline(a=0,b=1)
