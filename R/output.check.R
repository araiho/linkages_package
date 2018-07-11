output.check <- function(linkages.input, outdir){
  load(linkages.input)
  load(file.path(outdir,"linkages.out.Rdata"))


  sink(paste0(outdir,"/warnings.txt"))
  if(sum(ntrees.kill[,nyear,1]) == 0) {
    surv <- print('No survivors to final year')
  }
  if(max(ag.biomass) > 50 ) {
    bio <- print(paste('Large biomass. Possible there are too many species? Or need to calibrate parameters? Max Biomass =',
                                        max(ag.biomass)))
  }
  if(max(iage.save) < nyear/3) {
    age <- print(paste('Low ages. Max Age =', max(iage.save)))
  }
  sink()
}
