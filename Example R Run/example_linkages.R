##'
##' @title Example R script using example input data
##'
##' @param linkages.input is Rdata input file
##' @param outdir is the directory where you want your results to end up
##'

#load package
library(linkages)

#load input data
source("/Users/paleolab/linkages_package/Modified Fortran Code/write_txt_files.R")

#run model
Rprof(interval=.005)
linkages(linkages.input = "~/linkages_package/linkages.input.Rdata",
         outdir = "~/linkages_package/Example R Run/")
Rprof(NULL)
summaryRprof()


#load results
load("~/linkages_package/Example R Run/linkages.out.Rdata")

plot(ag.biomass,xlab='Time',ylab='Biomass (kgC/m2)')

#### Tradeoff Plots
par(mfrow=c(4,2))
mat.use <- gf.vec.save[1,,,1]
mat.use[is.na(mat.use)] <- 999
plot(bar[1,,1],col = rainbow(4,alpha = .5)[apply(mat.use,2,which.min)],pch=19,ylim=range(bar[1,,1]),main = spp.params[1,1])
matplot(t(gf.vec.save[1,,,1]),type='l',lty = 1,lwd=2, col = rainbow(4,alpha = .5))
for(i in 2:14){
  mat.use <- gf.vec.save[i,,,1]
  mat.use[is.na(mat.use)] <- 999
  plot(bar[i,,1],col = rainbow(4,alpha = .5)[apply(mat.use,2,which.min)],pch=19,ylim=range(bar[i,,1]),main = spp.params[i,1])
  if(length(which(!is.na(gf.vec.save[i,,,1])))>1){
    matplot(t(gf.vec.save[i,,,1]),type='l',lty = 1,lwd=2, col = rainbow(4,alpha = .5))

  }else{
    plot.new()
  }
}
plot.new()
legend('center',c('light','moisture','nitrogen','degree day'),
       pch = 19, col = rainbow(4,alpha = .5),
       cex = .7,ncol=2)

