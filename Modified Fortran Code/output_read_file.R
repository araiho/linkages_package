#####

##### SECOND WRITE look at the OUT.csv file to see what LINKAGES models

#####

################ use terminal to compile linkages.f

link = as.matrix(read.csv(paste0(outdir,"/OUT.csv"),head=FALSE))

head(link)

#####
##### Look at biomass of different species
#####

biomass_values=as.matrix(link[505:755,1:10])
#biomass_cis = link[2496:nrow(link),1:10]
x=seq(0,nyear,kprnt)
quartz()
par(mfrow=c(1,2))
plot(x,biomass_values[,2],type="l",lwd=4,main=NA,xlab="Years",ylab="Average Biomass",ylim=c(0,max(biomass_values[,2:nspec])))

lines(x,biomass_values[,3],col="red",lwd=4)
lines(x,biomass_values[,4],col="yellow",lwd=4)
lines(x,biomass_values[,5],col="blue",lwd=4)
lines(x,biomass_values[,6],col="green",lwd=4)
lines(x,biomass_values[,7],col="purple",lwd=4)
lines(x,biomass_values[,8],col="gray",lwd=4)
lines(x,biomass_values[,9],col="orange",lwd=4)
lines(x,biomass_values[,10],col="lightblue",lwd=4)
plot.new()
legend("center",as.character(all_spp_params[which(all_spp_params$Spp_Number%in%pick_spp),1]),lwd=rep(4,9),lty=rep(1,9),
       col=c("black","red","yellow","blue","green","purple","gray","orange","lightblue","pink"),xpd=TRUE)

library(lattice)
library(stats)

par(mfrow=c(3,4))
for(i in 2:nspec){
  plot(biomass_values[,i],typ="l",ylim=c(min(biomass_values[,i]-biomass_cis[,i]),
                                         max(biomass_values[,i]+biomass_cis[,i])),
       main=as.character(all_spp_params[which(all_spp_params$Spp_Number%in%pick_spp),1])[i],ylab="Biomass")
  lines(biomass_values[,i]-biomass_cis[,i],lty=3,col="blue")
  lines(biomass_values[,i]+biomass_cis[,i],lty=3,col="blue")
}

#####
##### Look at the ecosystem parameter output #####
#####

ecosystem_params=link[1:nyear+1,]
colnames(ecosystem_params) = c("year","num stems","ag biomass","leaf litter","leaf litter N","ag npp","avail n","humus C:N","soil co2-c","soil OM","aet")
params_cis = link[nyear+2:502,]

quartz()
par(mfrow=c(3,3))
for(i in 2:10){
  plot(x,ecosystem_params[,i],typ="l",ylim=c(min(ecosystem_params[,i]),max(ecosystem_params[,i])),main=colnames(ecosystem_params)[i],ylab=NA,xlab="Year")
  #lines(x,ecosystem_params[,i]-params_cis[,i],lty=3,col="blue")
  #lines(x,ecosystem_params[,i]+params_cis[,i],lty=3,col="blue")
}




#pairs(ecosystem_params)

#common
#*Acer rubrum 3
#*Acer saccharum 4
#*Quercus rubra 44
#*Pinus strobus 33
#Quercus alba 37
#Quercus velutina 47
#Quercus coccinea 38
#*Tsuga canadensis 50
#*Fagus grandifolia 16
#*Betula alleghaniensis 69
#*Betula lenta 6
#Betula papyrifera 55

3,4,44,33,47,38,50,16,69,13


#Rare, but relevant in STEPPS1
#Castanea dentata 13
#Picea rubens 29
#Carya ovata 10
#Carya glabra 8

#Other (in STEPPS)
#Fraxinus americana 17
#Prunus serotina 68
#Ostrya virginiana 67
#Populus tremuloides 60