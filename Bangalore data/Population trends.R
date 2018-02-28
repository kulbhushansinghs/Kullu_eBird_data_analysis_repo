library(auk)
library(dplyr)
setwd("/home/kullu/Desktop/Link to Kullu_desktop/Git/Kullu_eBird_data_analysis_repo/Bangalore data")
dat<-read.csv("ebd_IN-KA-BN_prv_relNov-2017.txt", header = T, sep = "\t", fill = T )
summary(dat)
names(dat)
table(dat$LOCALITY)
dat.Jakkur.pstork<-subset(dat, 
                          dat$COMMON.NAME == "Painted Stork" & 
                            dat$LOCALITY == "Jakkur Lake, Bangalore")


