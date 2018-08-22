## adding libraries
library(auk)
library(dplyr)
library(mgcv)

## Setting working directory and importing data
setwd("/Users/Kullu/Dropbox (Snow Leopard Trust)/Kullu_desktop/Git/Kullu_eBird_data_analysis_repo/Jakkur data/ebd_paisto1_prv_relMay-2018")
dat<-read.csv("ebd_paisto1_prv_relMay-2018.txt", header = T, sep = "\t", fill = T )


## Organizing date
dat$Juliandate <- julian(as.Date(dat$OBSERVATION.DATE), 
                                origin = as.Date("2013-01-01"))

dat$OBSERVATION.DATE<-(as.Date(dat$OBSERVATION.DATE))
dat$OBSERVATION.COUNT <- as.numeric(as.character(dat$OBSERVATION.COUNT))

## Subsetting location
unique(dat$LOCALITY)
dat.jakkur <- subset(dat, dat$LOCALITY == "Jakkur Lake, Bangalore") ## Subsetting Jakkur data

thegam <- gam(OBSERVATION.COUNT[which(Juliandate>0)] ~ s(Juliandate[which(Juliandate>0)]), 
              data = dat.jakkur)

plot(thegam, residuals = T, main = "Painted Stork", 
     xlab = "Days from 1 Jan 2015")

abline(v = c(15, 15+365, 15+365*2, 15+365*3,15+365*4,15+365*5 ))
abline(v = c(150, 150+365, 150+365*2, 150+365*3,150+365*4,150+365*5 ))

plot(OBSERVATION.COUNT[which(OBSERVATION.DATE >= "2013-01-01")] ~ 
       OBSERVATION.DATE[which(OBSERVATION.DATE >= "2013-01-01")], 
     data = dat.jakkur, main = "Painted stork", pch = 16, ylim = c(0,100))
abline(v = as.Date("2014-01-01"))
abline(v = as.Date("2014-05-15"))
abline(v = as.Date("2015-01-01"))
abline(v = as.Date("2015-05-15"))
abline(v = as.Date("2016-01-01"))
abline(v = as.Date("2016-05-15"))
abline(v = as.Date("2017-01-01"))
abline(v = as.Date("2017-05-15"))
abline(v = as.Date("2018-01-01"))
abline(v = as.Date("2018-05-15"))


(as.Date(dat$OBSERVATION.DATE)) > "2015-01-01"
