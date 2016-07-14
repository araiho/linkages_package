rm(list=ls())

working_directory = paste0(getwd(),"/Modified Fortran Code")

site = "PHA" #don't change

#####

##### FIRST WRITE .txt files for LINKAGES

#####

#model2netcdf.LINKAGES(PFTs = pick_spp, outdir = outdir, sitelat = plat, sitelon = -plong, start_date=NULL, end_date=NULL,force=FALSE)

# met2model.LINKAGES(in.path = paste0("/Users/paleolab/Linkages/phase1a_met_drivers_v4.2/",site),
#                    in.prefix = site,
#                    outfolder = paste0("/Users/paleolab/linkages/UNDERC/old/"), #always need last "/"
#                    start_date = "0850/01/01",
#                    end_date = "2010/12/31",
#                    overwrite=FALSE,verbose=FALSE)

#####
##### Set up directories #####
#####

outdir = paste0(paste0(working_directory))
spp_list_site = read.csv(paste0(working_directory,"/spp_list_site.csv"),stringsAsFactors=FALSE)
texture =  read.csv(paste0(working_directory,"/texture.csv"))
env_drivers = read.csv(paste0(working_directory,"/PalEON_Phase1a_sites.csv"),stringsAsFactors=FALSE)

pick_site1 = which(colnames(spp_list_site)==site)

#####
##### Set up printing intervals DONT CHANGE #####
#####

kprnt = 1 #year interval for output
klast = 2 #number of plots
nyear = 10 #number of years to simulate 500 spin up + 1160 met data
ipolat_nums = seq(2,nyear,3) #years for climate interpolation
ipolat = length(ipolat_nums)-1 #number of years for climate interpolation

###### Read climate data created from MIP drivers #####
#climate_dat = read.csv(paste0(working_directory,"/Harvard Forest/climate.txt"),stringsAsFactors=FALSE)

set.seed(1)
temp_vec = c(-6.3,-4.7,-0.3,6.6,12.7,17.7,20.5,19.5,14.7,8.0,2.9,-3.0)
temp_means = matrix(round(rnorm(12*ipolat,temp_vec,1),1),ipolat,12,byrow=TRUE)# monthly mean temperature
temp_sd = matrix(1,ipolat,12) #monthly temperature standard deviation
precip_vec = c(8.5,7.9,9.9,9.8,9.7,11.1,11.7,9.4,9.4,11.5,10.7,9.9)
precip_means = matrix(round(rnorm(12*ipolat,precip_vec,1),1),ipolat,12,byrow=TRUE) #monthly mean precipitation
precip_sd = temp_sd #monthly standard deviation precipitation

write.table(file=paste0(working_directory,"/climate.txt"),rbind(temp_means,temp_sd,precip_means,precip_sd),sep=",",col.names=FALSE,row.names=FALSE)
#file.show("test_text1.txt")


#####
##### Set initial conditions #####
#####

bgs = 127 #DOY to begin growing season
egs = 275 #DOY to end growing season

pick_site = which(colnames(env_drivers)==site)
plat = as.numeric(env_drivers[3,pick_site]) #latitude
plong = abs(as.numeric(env_drivers[2,pick_site])) #longitude

sand = as.numeric(env_drivers[24,pick_site])/100
clay = as.numeric(env_drivers[19,pick_site])/100

soil.texture <- function(sand,clay){
  silt = 1 - sand - clay

  sand.keep = which(texture$xsand < sand + .1 & texture$xsand > sand - .1)
  clay.keep = which(texture$xclay[sand.keep] < clay + .1 & texture$xclay[sand.keep] > clay - .1)
  silt.keep = which(texture$xsilt[sand.keep[clay.keep]] < silt + .1 & texture$xsilt[sand.keep[clay.keep]] > silt - .1)

  row.keep = sand.keep[clay.keep[silt.keep]]

  return(texture[round(mean(row.keep)),c(8,14)]*100) # might need to divide by 3 or something because linkages wants cm water/30cm soil...
}
fc = round(as.numeric(unlist(soil.texture(sand = sand, clay = clay)[2])),digits = 2)
dry = round(as.numeric(unlist(soil.texture(sand = sand, clay = clay)[1])),digits = 2)

#####
##### Write initial condition file #####
#####

sink(paste0(outdir,"/settings.txt"))
cat(kprnt,klast,nyear,sep=",")
cat("\n")
cat(ipolat)
cat("\n")
cat(ipolat_nums,sep=",")
cat("\n")
cat(plat,plong,bgs,egs,fc,dry,sep=",")
sink()

#####
##### Write species data table #####
#####

nspec = 9
bmspec = nspec
all_spp_params = read.csv(paste0(working_directory,"/spp_matrix.csv"))
pick_spp = c(1:9)
spp_params = all_spp_params[which(all_spp_params$Spp_Number%in%pick_spp),3:ncol(all_spp_params)]
spec_nums = all_spp_params[which(all_spp_params$Spp_Number%in%pick_spp),2]

sink(paste0(outdir,"/spp.txt"))
cat(nspec,bmspec,sep=",")
cat("\n")
cat(spec_nums)
cat("\n")
write.table(spp_params,sep=",",col.names=FALSE,row.names=FALSE)
sink()

#####
##### Write switch text file #####
#####

switch_chars_list = read.csv(paste0(working_directory,"/switch.csv"))
switch_chars = as.character(switch_chars_list[spec_nums,3])
sink(paste0(outdir,"/switch.txt"))
cat(switch_chars,sep="\n")
sink()

#####
##### Write underground parameters file #####
#####

NLVAR = 10
NLT = 17
NCOHRT = 1

init_litter_wt = c(rep(0,17)) #The weight of an incoming cohort of litter (initialized to zero)
init_perc_N = c(.0068,.0076,.0109,.0106,.0079,.0081,.0085,.0057,
                .0090,.0056,.0063,.0046,.0096,.0038,.0038,.0038,.0050) #Initial percent of nitrogen
g_N_per_g_wt_loss = c(.0251,.0315,.0574,.0377,.0256,.0286,.0336,
                      .0477,.0341,.0326,.0220,.0163,.0284,.0195,
                      .0195,.0195,.0364) #Grams of nitrogen immobilized per gram weight loss;
crit_perc_N = c(.0183,.0239,.0465,.0271,.0177,.0205,.0251,
                .0420,.0251,.0270,.0157,.0117,.0188,.0157,
                .0157,.0157,.0314) #Critical percent of nitrogen
litter_type = seq(1,17,1) #Litter type: 1 through 12 are the 12 leaf-litter types in order of decreasing decay rate and increasing nitrogen-immobilization rate and correspond to species parameter TL. Thirteen is root litter. Fourteen and fifteen are fresh wood from trees less than or greater than 10 cm dbh, respectively. Sixteen is twig litter. Seventeen is well-decayed wood not yet humus;
dest = c(1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,1,1) #Destination when cohort reaches critical percent to nitrogen (1 = humus; 2 = well-decayed wood);
init_perc_lignin = c(.039,.121,.193,.158,.187,.206,.214,
                     .241,.248,.280,.216,.283,.253,.173,
                     .173,.173,.423) #Initial percent of lignin;
lignin_decay_param1 = c(.5217,.5219,.7787,.6693,.5194,.6839,.7059,
                        1.1967,.6105,.5926,.9052,.5646,.7000,.4831,
                        .4831,.4831,.7222) #Lignin decay parameters [see Eq. B-8, Appendix 2, in Pastor and Post (1985)]
lignin_decay_param2 = c(.336,.400,.508,.435,.315,.475,.460,.790,.359,
                        .383,.594,.327,.456,.299,.299,.299,.299)
ash_corr_factor = c(.90,.90,.92,.92,.93,.96,.94,.91,.95,.97,.97,.96,.98,.99,.99,.96,.99)

dirt_params = cbind(init_litter_wt,init_perc_N,g_N_per_g_wt_loss,crit_perc_N,
                    litter_type,dest,init_perc_lignin,lignin_decay_param1,lignin_decay_param2,
                    ash_corr_factor)
basesc = 74. #starting humus weight
basesn = 1.640 #starting N content

sink(paste0(outdir,"/dirt.txt"))
cat(NLVAR,NLT,NCOHRT,sep=" ")
cat("\n")
write.table(dirt_params,sep=",",col.names=FALSE,row.names=FALSE)
cat("\n")
cat(basesc,basesn,sep=" ")
sink()

Rinput <- file.path(getwd(),"linkages.input1.Rdata")

iplot <- klast
max.ind <- 15000
temp.mat <- matrix((rep(t(temp_means),nyear)),nrow=nyear,ncol=12,byrow = TRUE)
precip.mat <- matrix((rep(t(precip_means),nyear)),nrow=nyear,ncol=12,byrow = TRUE)
spp.params <- spp_params
switch.mat <- read.csv("~/linkages_package/inst/switch.mat.csv")
switch.mat <- matrix(unlist(switch.mat),72,5)
switch.mat <- switch.mat[1:9,] #change if you change spp
fdat <- read.csv("~/linkages_package/inst/fdat.csv")
clat <- read.csv("~/linkages_package/inst/clat.csv")
start.year <- 1991
end.year <- 2000

save(iplot, nyear, nspec, fc, dry, bgs, egs, max.ind,
     plat, temp.mat, precip.mat, spp.params, switch.mat,
     fdat, clat, basesc, basesn, start.year, end.year, file = Rinput)

