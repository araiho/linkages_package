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
linkages(linkages.input = "/Users/paleolab/linkages_package/linkages.input1.Rdata",
         outdir = "/Users/paleolab/linkages_package/Example R Run/")

#load results
load("/Users/paleolab/linkages_package/Example R Run/linkages.out.Rdata")
