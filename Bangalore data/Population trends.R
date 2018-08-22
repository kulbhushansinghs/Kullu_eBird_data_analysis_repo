## adding libraries
library(auk)
library(dplyr)
library(mgcv)

## Setting working directory and importing data
setwd("/Users/Kullu/Dropbox (Snow Leopard Trust)/Kullu_desktop/Git/Kullu_eBird_data_analysis_repo/Bangalore data")
dat<-read.csv("ebd_IN-KA-BN_prv_relNov-2017.txt", header = T, sep = "\t", fill = T )
dat<- filter(dat, CATEGORY == "species", ALL.SPECIES.REPORTED == 1)

## Understanding the data
summary(dat)
names(dat)
table(dat$LOCALITY)

## Subsetting for Jukkur and removing other data
dat.jakkur <- subset(dat, dat$LOCALITY == "Jakkur Lake, Bangalore") ## Subsetting Jakkur data
rm(dat)

## Organizing date
dat.jakkur$Juliandate <- julian(as.Date(dat.jakkur$OBSERVATION.DATE), 
                                       origin = as.Date("2011-01-01"))
dat.jakkur$OBSERVATION.COUNT <- as.numeric(as.character(dat.jakkur$OBSERVATION.COUNT))

## Exploration
length(unique(dat.jakkur$SCIENTIFIC.NAME))

for(i in 1:length(unique(dat.jakkur$COMMON.NAME))){
dat.jakkur.species<-subset(dat.jakkur, 
                          dat.jakkur$COMMON.NAME == unique(dat.jakkur$COMMON.NAME)[i], 
                          select = c(OBSERVATION.DATE, OBSERVATION.COUNT, COMMON.NAME, Juliandate))

if(length(dat.jakkur.species$COMMON.NAME) <= 10000 & length(dat.jakkur.species$COMMON.NAME) >= 10){
thegam <- gam(OBSERVATION.COUNT[which(Juliandate>0)] ~ s(Juliandate[which(Juliandate>0)]), 
              data = dat.jakkur.species)
plot(thegam, residuals = T, main = unique(dat.jakkur$COMMON.NAME)[i], 
     xlab = "Days from 1 Jan 2011")
plot(OBSERVATION.COUNT[which(Juliandate>0)] ~ Juliandate[which(Juliandate>0)], 
     data = dat.jakkur.species, main = unique(dat.jakkur$COMMON.NAME)[i], 
     xlim = c(0, 2200))
abline(v = 0)
abline(v = 365)
abline(v = 365*2)
abline(v = 365*3)
abline(v = 365*4)
abline(v = 365*5)
abline(v = 365*6)
abline(v = 365*7)
abline(v = 365*8)
}
}

summary(dat.Jakkur.pstork)
dat.Jakkur.pstork$OBSERVATION.COUNT <- as.numeric(as.character(dat.Jakkur.pstork$OBSERVATION.COUNT))
hist(dat.Jakkur.pstork$OBSERVATION.COUNT)


with(dat.Jakkur.pstork, plot(Juliandate[which(Juliandate>0)] , 
                             OBSERVATION.COUNT[which(Juliandate>0)]))
with(dat.Jakkur.pstork, 
     abline(lm(OBSERVATION.COUNT[which(Juliandate>0)]~Juliandate[which(Juliandate>0)])))
thegam <- gam(OBSERVATION.COUNT[which(Juliandate>0)]~ s(Juliandate[which(Juliandate>0)]), 
              data = dat.Jakkur.pstork)
plot(thegam, residuals = T, main = "test")
