#############
## adding libraries
library(auk)
library(dplyr)
library(mgcv)
library(ggplot2)
## Setting working directory and importing data
setwd("/Users/Kullu/Dropbox (Snow Leopard Trust)/Kullu_desktop/Git/Kullu_eBird_data_analysis_repo/Jakkur data/ebd_paisto1_relAug-2018")
dat<-read.csv("ebd_paisto1_relAug-2018.txt", header = T, sep = "\t", fill = T )
setwd("/Users/Kullu/Dropbox (Snow Leopard Trust)/Kullu_desktop/Git/Kullu_eBird_data_analysis_repo/Jakkur data/ebd_spbpel1_relAug-2018")
dat.peli<- read.csv("ebd_spbpel1_relAug-2018.txt", header = T, sep = "\t", fill = T )



## Organizing date
dat$Juliandate <- julian(as.Date(dat$OBSERVATION.DATE), 
                                origin = as.Date("2013-01-01"))

dat$OBSERVATION.DATE<-(as.Date(dat$OBSERVATION.DATE))
dat$OBSERVATION.COUNT <- as.numeric(as.character(dat$OBSERVATION.COUNT))
dat.peli$OBSERVATION.DATE<-(as.Date(dat.peli$OBSERVATION.DATE))
dat.peli$OBSERVATION.COUNT <- as.numeric(as.character(dat.peli$OBSERVATION.COUNT))


## Subsetting location for Jakkur
unique(dat$LOCALITY)
dat.jakkur <- subset(dat, dat$LOCALITY == "Jakkur Lake, Bangalore") ## Subsetting Jakkur data
dat.peli.jakkur <- subset(dat.peli, dat.peli$LOCALITY == "Jakkur Lake, Bangalore") ## Subsetting Jakkur data
## Plotting for painted stork in Jakkur
dat.jakkur.plot<- subset(dat.jakkur, OBSERVATION.DATE > "2014-01-01")

plot1<- ggplot(dat.jakkur.plot, 
               aes (x = OBSERVATION.DATE, y = OBSERVATION.COUNT)) +
  geom_rect(mapping = aes(xmin = as.Date("2014-01-15"), 
                          xmax = as.Date("2014-06-15"), ymin = 0, ymax = 200, fill = t), 
            fill = 7,  alpha = 0.005)+
  geom_rect(mapping = aes(xmin = as.Date("2015-01-15"), 
                          xmax = as.Date("2015-06-15"), ymin = 0, ymax = 200, fill = t), 
            fill = 7,  alpha = 0.005)+
  geom_rect(mapping = aes(xmin = as.Date("2016-01-15"), 
                          xmax = as.Date("2016-06-15"), ymin = 0, ymax = 200, fill = t), 
            fill = 7,  alpha = 0.005)+
  geom_rect(mapping = aes(xmin = as.Date("2017-01-15"), 
                          xmax = as.Date("2017-06-15"), ymin = 0, ymax = 200, fill = t), 
            fill = 7,  alpha = 0.005)+
  geom_rect(mapping = aes(xmin = as.Date("2018-01-15"), 
                          xmax = as.Date("2018-06-15"), ymin = 0, ymax = 200, fill = t), 
            fill = 7,  alpha = 0.005)+
  theme_gray(base_size = 12)+
  geom_point(alpha = 0.5, color = "blue", size = 3)+
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), color = "black", alpha = 0.7) + 
  labs(x = "Date", y = "Observation count")
  
plot1

## plotting pelican for Jakkur

dat.peli.plot<- subset(dat.peli.jakkur, OBSERVATION.DATE > "2014-01-01")

plot2<- ggplot(dat.peli.plot, 
               aes (x = OBSERVATION.DATE, y = OBSERVATION.COUNT)) +
  geom_rect(mapping = aes(xmin = as.Date("2014-01-15"), 
                          xmax = as.Date("2014-06-15"), ymin = 0, ymax = 250, fill = t), 
            fill = 7,  alpha = 0.005)+
  geom_rect(mapping = aes(xmin = as.Date("2015-01-15"), 
                          xmax = as.Date("2015-06-15"), ymin = 0, ymax = 250, fill = t), 
            fill = 7,  alpha = 0.005)+
  geom_rect(mapping = aes(xmin = as.Date("2016-01-15"), 
                          xmax = as.Date("2016-06-15"), ymin = 0, ymax = 250, fill = t), 
            fill = 7,  alpha = 0.005)+
  geom_rect(mapping = aes(xmin = as.Date("2017-01-15"), 
                          xmax = as.Date("2017-06-15"), ymin = 0, ymax = 250, fill = t), 
            fill = 7,  alpha = 0.005)+
  geom_rect(mapping = aes(xmin = as.Date("2018-01-15"), 
                          xmax = as.Date("2018-06-15"), ymin = 0, ymax = 250, fill = t), 
            fill = 7,  alpha = 0.005)+
  theme_gray(base_size = 12)+
  geom_point(alpha = 0.5, color = "blue", size = 3)+
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), color = "black", alpha = 0.7) + 
  labs(x = "Date", y = "Observation count") +
  ylim(0, 250)

plot2


## plotting painted stork for Hoskote
dat.hoskote <- subset(dat, dat$LOCALITY == "Hoskote Lake") 

dat.hoskote.plot<- subset(dat.hoskote, OBSERVATION.DATE >= "2014-01-01")

plot3<- ggplot(dat.hoskote.plot, 
               aes (x = OBSERVATION.DATE, y = OBSERVATION.COUNT)) +
  geom_rect(mapping = aes(xmin = as.Date("2014-01-15"), 
                          xmax = as.Date("2014-06-15"), ymin = 0, ymax = 120, fill = t), 
            fill = 7,  alpha = 0.005)+
  geom_rect(mapping = aes(xmin = as.Date("2015-01-15"), 
                          xmax = as.Date("2015-06-15"), ymin = 0, ymax = 120, fill = t), 
            fill = 7,  alpha = 0.005)+
  geom_rect(mapping = aes(xmin = as.Date("2016-01-15"), 
                          xmax = as.Date("2016-06-15"), ymin = 0, ymax = 120, fill = t), 
            fill = 7,  alpha = 0.005)+
  geom_rect(mapping = aes(xmin = as.Date("2017-01-15"), 
                          xmax = as.Date("2017-06-15"), ymin = 0, ymax = 120, fill = t), 
            fill = 7,  alpha = 0.005)+
  geom_rect(mapping = aes(xmin = as.Date("2018-01-15"), 
                          xmax = as.Date("2018-06-15"), ymin = 0, ymax = 120, fill = t), 
            fill = 7,  alpha = 0.005)+
  theme_gray(base_size = 12)+
  geom_point(alpha = 0.5, color = "blue", size = 3)+
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), color = "black", alpha = 0.7) + 
  labs(x = "Date", y = "Observation count")

plot3

## Exploratory analysis

ggplot(dat.jakkur.plot, aes(x = EFFORT.DISTANCE.KM, y = OBSERVATION.COUNT)) + 
  geom_point()

ggplot(dat.jakkur.plot, aes(x = DURATION.MINUTES, y = OBSERVATION.COUNT)) + 
  geom_point()

ggplot(dat.peli.plot, aes(x = EFFORT.DISTANCE.KM, y = OBSERVATION.COUNT)) + 
  geom_point()

ggplot(dat.peli.plot, aes(x = DURATION.MINUTES, y = OBSERVATION.COUNT)) + 
  geom_point()

ggplot(dat.hoskote.plot, aes(x = EFFORT.DISTANCE.KM, y = OBSERVATION.COUNT)) + 
  geom_point()

ggplot(dat.hoskote.plot, aes(x = DURATION.MINUTES, y = OBSERVATION.COUNT)) + 
  geom_point()

##########################################################################################
thegam <- gam(OBSERVATION.COUNT[which(Juliandate>0)] ~ s(Juliandate[which(Juliandate>0)]), 
              data = dat.hoskote)

plot(thegam, residuals = T, main = "Painted Stork", 
     xlab = "Days from 1 Jan 2015")

abline(v = c(15, 15+365, 15+365*2, 15+365*3,15+365*4,15+365*5 ))
abline(v = c(150, 150+365, 150+365*2, 150+365*3,150+365*4,150+365*5 ))

plot(OBSERVATION.COUNT[which(OBSERVATION.DATE >= "2013-01-01")] ~ 
       OBSERVATION.DATE[which(OBSERVATION.DATE >= "2013-01-01")], 
     data = dat.hoskote, main = "Painted stork", pch = 16, ylim = c(0,100))
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

## Subsetting Hessaraghatta Lake

dat.hesargatta <- subset(dat, dat$LOCALITY == "Hessaraghatta Lake" | dat$LOCALITY == "Hesaraghatta--Hesaraghatta Grassland") ## Subsetting Jakkur data

thegam <- gam(OBSERVATION.COUNT[which(Juliandate>0)] ~ s(Juliandate[which(Juliandate>0)]), 
              data = dat.hesargatta)

plot(thegam, residuals = T, main = "Painted Stork", 
     xlab = "Days from 1 Jan 2015")

abline(v = c(15, 15+365, 15+365*2, 15+365*3,15+365*4,15+365*5 ))
abline(v = c(150, 150+365, 150+365*2, 150+365*3,150+365*4,150+365*5 ))

plot(OBSERVATION.COUNT[which(OBSERVATION.DATE >= "2013-01-01")] ~ 
       OBSERVATION.DATE[which(OBSERVATION.DATE >= "2013-01-01")], 
     data = dat.hesargatta, main = "Painted stork", pch = 16, ylim = c(0,100))
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

## Subsetting Yelahanka Kere, Bangalore
dat.yelahanka <- subset(dat, dat$LOCALITY == "Yelahanka Kere, Bangalore") ## Subsetting Jakkur data

thegam <- gam(OBSERVATION.COUNT[which(Juliandate>0)] ~ s(Juliandate[which(Juliandate>0)]), 
              data = dat.yelahanka)

plot(thegam, residuals = T, main = "Painted Stork", 
     xlab = "Days from 1 Jan 2015")

abline(v = c(15, 15+365, 15+365*2, 15+365*3,15+365*4,15+365*5 ))
abline(v = c(150, 150+365, 150+365*2, 150+365*3,150+365*4,150+365*5 ))

plot(OBSERVATION.COUNT[which(OBSERVATION.DATE >= "2013-01-01")] ~ 
       OBSERVATION.DATE[which(OBSERVATION.DATE >= "2013-01-01")], 
     data = dat.yelahanka, main = "Painted stork", pch = 16, ylim = c(0,100))
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

## Subsetting Hebbal Lake, Bangalore
dat.hebbal <- subset(dat, dat$LOCALITY == "Hebbal Lake, Bangalore") ## Subsetting Jakkur data

thegam <- gam(OBSERVATION.COUNT[which(Juliandate>0)] ~ s(Juliandate[which(Juliandate>0)]), 
              data = dat.hebbal)

plot(thegam, residuals = T, main = "Painted Stork", 
     xlab = "Days from 1 Jan 2015")

abline(v = c(15, 15+365, 15+365*2, 15+365*3,15+365*4,15+365*5 ))
abline(v = c(150, 150+365, 150+365*2, 150+365*3,150+365*4,150+365*5 ))

plot(OBSERVATION.COUNT[which(OBSERVATION.DATE >= "2013-01-01")] ~ 
       OBSERVATION.DATE[which(OBSERVATION.DATE >= "2013-01-01")], 
     data = dat.hebbal, main = "Painted stork", pch = 16, ylim = c(0,100))
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

## Subsetting Doddanakundi Lake
dat.doddanakundi <- subset(dat, dat$LOCALITY == "Doddanakundi Lake") ## Subsetting Jakkur data

thegam <- gam(OBSERVATION.COUNT[which(Juliandate>0)] ~ s(Juliandate[which(Juliandate>0)]), 
              data = dat.doddanakundi)

plot(thegam, residuals = T, main = "Painted Stork", 
     xlab = "Days from 1 Jan 2015")

abline(v = c(15, 15+365, 15+365*2, 15+365*3,15+365*4,15+365*5 ))
abline(v = c(150, 150+365, 150+365*2, 150+365*3,150+365*4,150+365*5 ))

plot(OBSERVATION.COUNT[which(OBSERVATION.DATE >= "2013-01-01")] ~ 
       OBSERVATION.DATE[which(OBSERVATION.DATE >= "2013-01-01")], 
     data = dat.doddanakundi, main = "Painted stork", pch = 16, ylim = c(0,100))
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

## Subsetting Doddabommasandra Lake
dat.doddabommasandra <- subset(dat, dat$LOCALITY == "Doddabommasandra Lake") ## Subsetting Jakkur data

thegam <- gam(OBSERVATION.COUNT[which(Juliandate>0)] ~ s(Juliandate[which(Juliandate>0)]), 
              data = dat.doddabommasandra)

plot(thegam, residuals = T, main = "Painted Stork", 
     xlab = "Days from 1 Jan 2015")

abline(v = c(15, 15+365, 15+365*2, 15+365*3,15+365*4,15+365*5 ))
abline(v = c(150, 150+365, 150+365*2, 150+365*3,150+365*4,150+365*5 ))

plot(OBSERVATION.COUNT[which(OBSERVATION.DATE >= "2013-01-01")] ~ 
       OBSERVATION.DATE[which(OBSERVATION.DATE >= "2013-01-01")], 
     data = dat.doddabommasandra, main = "Painted stork", pch = 16, ylim = c(0,100))
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


## Kaikondrahalli Kere, Bangalore
dat.kaikondrahalli <- subset(dat, dat$LOCALITY == "Kaikondrahalli Kere, Bangalore") ## Subsetting Jakkur data

thegam <- gam(OBSERVATION.COUNT[which(Juliandate>0)] ~ s(Juliandate[which(Juliandate>0)]), 
              data = dat.kaikondrahalli)

plot(thegam, residuals = T, main = "Painted Stork", 
     xlab = "Days from 1 Jan 2015")

abline(v = c(15, 15+365, 15+365*2, 15+365*3,15+365*4,15+365*5 ))
abline(v = c(150, 150+365, 150+365*2, 150+365*3,150+365*4,150+365*5 ))

plot(OBSERVATION.COUNT[which(OBSERVATION.DATE >= "2013-01-01")] ~ 
       OBSERVATION.DATE[which(OBSERVATION.DATE >= "2013-01-01")], 
     data = dat.kaikondrahalli, main = "Painted stork", pch = 16, ylim = c(0,100))
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

## Madivala Lake, Bangalore
dat.madivala <- subset(dat, dat$LOCALITY == "Madivala Lake, Bangalore") ## Subsetting Jakkur data

thegam <- gam(OBSERVATION.COUNT[which(Juliandate>0)] ~ s(Juliandate[which(Juliandate>0)]), 
              data = dat.madivala)

plot(thegam, residuals = T, main = "Painted Stork", 
     xlab = "Days from 1 Jan 2015")

abline(v = c(15, 15+365, 15+365*2, 15+365*3,15+365*4,15+365*5 ))
abline(v = c(150, 150+365, 150+365*2, 150+365*3,150+365*4,150+365*5 ))

plot(OBSERVATION.COUNT[which(OBSERVATION.DATE >= "2013-01-01")] ~ 
       OBSERVATION.DATE[which(OBSERVATION.DATE >= "2013-01-01")], 
     data = dat.madivala, main = "Painted stork", pch = 16, ylim = c(0,100))
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

## Hulimavu Kere, Bangalore: Too few painted stork sightings
dat.hulimavu <- subset(dat, dat$LOCALITY == "Hulimavu Kere, Bangalore") ## Subsetting Jakkur data

thegam <- gam(OBSERVATION.COUNT[which(Juliandate>0)] ~ s(Juliandate[which(Juliandate>0)]), 
              data = dat.hulimavu)

plot(thegam, residuals = T, main = "Painted Stork", 
     xlab = "Days from 1 Jan 2015")

abline(v = c(15, 15+365, 15+365*2, 15+365*3,15+365*4,15+365*5 ))
abline(v = c(150, 150+365, 150+365*2, 150+365*3,150+365*4,150+365*5 ))

plot(OBSERVATION.COUNT[which(OBSERVATION.DATE >= "2013-01-01")] ~ 
       OBSERVATION.DATE[which(OBSERVATION.DATE >= "2013-01-01")], 
     data = dat.hulimavu, main = "Painted stork", pch = 16, ylim = c(0,100))
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

## Puttenhalli Lake (Puttakere): Only one sighting
dat.puttenhalli <- subset(dat, dat$LOCALITY == "Puttenhalli Lake (Puttakere)") ## Subsetting Jakkur data

thegam <- gam(OBSERVATION.COUNT[which(Juliandate>0)] ~ s(Juliandate[which(Juliandate>0)]), 
              data = dat.puttenhalli)

plot(thegam, residuals = T, main = "Painted Stork", 
     xlab = "Days from 1 Jan 2015")

abline(v = c(15, 15+365, 15+365*2, 15+365*3,15+365*4,15+365*5 ))
abline(v = c(150, 150+365, 150+365*2, 150+365*3,150+365*4,150+365*5 ))

plot(OBSERVATION.COUNT[which(OBSERVATION.DATE >= "2013-01-01")] ~ 
       OBSERVATION.DATE[which(OBSERVATION.DATE >= "2013-01-01")], 
     data = dat.puttenhalli, main = "Painted stork", pch = 16, ylim = c(0,100))
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

## Hennagara Lake
dat.hennagara <- subset(dat, dat$LOCALITY == "Hennagara Lake") ## Subsetting Jakkur data

thegam <- gam(OBSERVATION.COUNT[which(Juliandate>0)] ~ s(Juliandate[which(Juliandate>0)]), 
              data = dat.hennagara)

plot(thegam, residuals = T, main = "Painted Stork", 
     xlab = "Days from 1 Jan 2015")

abline(v = c(15, 15+365, 15+365*2, 15+365*3,15+365*4,15+365*5 ))
abline(v = c(150, 150+365, 150+365*2, 150+365*3,150+365*4,150+365*5 ))

plot(OBSERVATION.COUNT[which(OBSERVATION.DATE >= "2013-01-01")] ~ 
       OBSERVATION.DATE[which(OBSERVATION.DATE >= "2013-01-01")], 
     data = dat.hennagara, main = "Painted stork", pch = 16, ylim = c(0,100))
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

## Muthanallur Lake, Bangalore
dat.muthnallur <- subset(dat, dat$LOCALITY == "Muthanallur Lake, Bangalore") ## Subsetting Jakkur data

thegam <- gam(OBSERVATION.COUNT[which(Juliandate>0)] ~ s(Juliandate[which(Juliandate>0)]), 
              data = dat.muthnallur)

plot(thegam, residuals = T, main = "Painted Stork", 
     xlab = "Days from 1 Jan 2015")

abline(v = c(15, 15+365, 15+365*2, 15+365*3,15+365*4,15+365*5 ))
abline(v = c(150, 150+365, 150+365*2, 150+365*3,150+365*4,150+365*5 ))

plot(OBSERVATION.COUNT[which(OBSERVATION.DATE >= "2013-01-01")] ~ 
       OBSERVATION.DATE[which(OBSERVATION.DATE >= "2013-01-01")], 
     data = dat.muthnallur, main = "Painted stork", pch = 16, ylim = c(0,100))
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

## Bharatpur--Keoladeo Ghana NP
dat.bharatpur <- subset(dat, dat$LOCALITY == "Bharatpur--Keoladeo Ghana NP") ## Subsetting Jakkur data

thegam <- gam(OBSERVATION.COUNT[which(Juliandate>0)] ~ s(Juliandate[which(Juliandate>0)]), 
              data = dat.bharatpur)

plot(thegam, residuals = T, main = "Painted Stork", 
     xlab = "Days from 1 Jan 2015")

abline(v = c(15, 15+365, 15+365*2, 15+365*3,15+365*4,15+365*5 ))
abline(v = c(150, 150+365, 150+365*2, 150+365*3,150+365*4,150+365*5 ))

plot(OBSERVATION.COUNT[which(OBSERVATION.DATE >= "2013-01-01")] ~ 
       OBSERVATION.DATE[which(OBSERVATION.DATE >= "2013-01-01")], 
     data = dat.bharatpur, main = "Painted stork", pch = 16, ylim = c(0,250))
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

##Sultanpur National Park
dat.sultanpur <- subset(dat, dat$LOCALITY == "Sultanpur National Park") ## Subsetting Jakkur data

thegam <- gam(OBSERVATION.COUNT[which(Juliandate>0)] ~ s(Juliandate[which(Juliandate>0)]), 
              data = dat.sultanpur)

plot(thegam, residuals = T, main = "Painted Stork", 
     xlab = "Days from 1 Jan 2015")

abline(v = c(15, 15+365, 15+365*2, 15+365*3,15+365*4,15+365*5 ))
abline(v = c(150, 150+365, 150+365*2, 150+365*3,150+365*4,150+365*5 ))

plot(OBSERVATION.COUNT[which(OBSERVATION.DATE >= "2013-01-01")] ~ 
       OBSERVATION.DATE[which(OBSERVATION.DATE >= "2013-01-01")], 
     data = dat.sultanpur, main = "Painted stork", pch = 16, ylim = c(0,250))
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

## Analyszing nesting code
dat.nesting <- subset(dat, dat$BREEDING.BIRD.ATLAS.CODE == "FL" |
                        dat$BREEDING.BIRD.ATLAS.CODE == "FY" |
                        dat$BREEDING.BIRD.ATLAS.CODE == "NY" |
                        dat$BREEDING.BIRD.ATLAS.CODE == "ON" |
                        dat$BREEDING.BIRD.ATLAS.CODE == "NE" |
                        dat$BREEDING.BIRD.ATLAS.CODE == "NB" )
dat.nesting<-subset(dat.nesting, dat.nesting$COUNTY == "Bangalore")
table(dat.nesting$LOCALITY)
