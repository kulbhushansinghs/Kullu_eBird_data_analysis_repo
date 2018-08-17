#Libraries
require(dplyr)
require(tidyr)
require(reshape2)
library(ggplot2)

setwd("/Users/Kullu/Dropbox (Snow Leopard Trust)/Kullu_desktop/Git/Kullu_eBird_data_analysis_repo/Jakkur data")
dat.nest <- read.csv("Jukkur painted Stork nest data.csv")
dat.pop <- read.csv("Jukkur painted Stork population data.csv")

## Fixing the date columns
dat.nest$Survey.date <- as.Date(dat.nest$Survey.date, format = "%d/%m/%Y")
dat.pop$Date <- as.Date(dat.pop$Date, format = "%d/%m/%y")

## Analysing the Population data
plot(dat.pop$Date, dat.pop$Number.of.nests, 
     lwd = 2, type = "l", ylim = c(0,210),
     xlab = "Month", ylab = "Frequency", bty = "l")
points(dat.pop$Date, dat.pop$Number.of.nests,cex = 1, pch = 16)
points(dat.pop$Date, dat.pop$Number.of.chicks, 
       lwd = 2, type = "l", lty = 2)
points(dat.pop$Date, dat.pop$Number.of.chicks, pch = 16)
points(dat.pop$Date, dat.pop$Number.of.adults, lwd = 2, type = "l", lty = 3, col = "red")
points(dat.pop$Date, dat.pop$Number.of.adults, pch = 16, col = "red")

## plotting in ggplot2
dat.pop.long <- melt(dat.pop, id.vars = "Date", 
                     variable.name = "observation", value.name = "Number")
dat.pop.long <- subset(dat.pop.long, dat.pop.long$observation != "Remarks")
dat.pop.long$Number <- as.numeric(as.character(dat.pop.long$Number))

ggplot(dat.pop.long, aes(x = Date, y = Number, colour = observation)) +
  geom_line(size = 0.8) + geom_point(size = 2) + theme_gray(base_size = 12) +
  scale_color_manual(values = c("forest green", "blue", "red"), name = "Observation", 
                     labels = c("Total nests", "Total chicks", "Total adults")) +
  labs(x = "Month", y = "Numbers") 

## Analysing the nesting data
table(dat.nest$Survey.date, dat.nest$State.of.the.nest)

dat.nest.wide<-dcast(dat.nest, formula = Survey.date ~ State.of.the.nest)
dat.nest.wide$Total.nests <- (dat.nest.wide$`Black chicks`+
                                dat.nest.wide$Fledgling + 
                                dat.nest.wide$Incubation + 
                                dat.nest.wide$`Nest building` +
                                dat.nest.wide$`Not detected` + 
                                dat.nest.wide$`Standing chicks`)
dat.nest.wide$Total.nestlings <- (dat.nest.wide$`Standing chicks` + 
                                    dat.nest.wide$`Black chicks`)


par(xpd = F)
par(mar  = c(5,5,1,5))
plot(dat.nest.wide$Survey.date, dat.nest.wide$Total.nests, 
     type = "l", ylim = c(0,50), lwd = 2, ylab = "Number of nests", xlab = "Months", bty = "L")

points(dat.nest.wide$Survey.date, dat.nest.wide$Incubation, 
     type = "l", lwd = 2, lty = 2, col = "forest green")
points(dat.nest.wide$Survey.date, dat.nest.wide$Incubation, 
      col = "forest green", pch = 16)
points(dat.nest.wide$Survey.date, dat.nest.wide$Fledgling, 
       type = "l", lwd = 2, lty = 3, col = "dark blue")
points(dat.nest.wide$Survey.date, dat.nest.wide$Fledgling, 
      pch = 16, col = "dark blue")
points(dat.nest.wide$Survey.date, dat.nest.wide$`Not detected`, 
       type = "l", lwd = 2, lty = 5, col = "red")
points(dat.nest.wide$Survey.date, dat.nest.wide$`Not detected`, 
       pch = 16, col = "red")
points(dat.nest.wide$Survey.date, (dat.nest.wide$`Black chicks` +
         dat.nest.wide$`Standing chicks`), 
       type = "l", lwd = 2, lty = 4, col = "blue")
points(dat.nest.wide$Survey.date, (dat.nest.wide$`Black chicks` +
                                     dat.nest.wide$`Standing chicks`), 
       pch = 16, lty = 4, col = "blue")

legend(as.Date("15/04/18", format = "%d/%m/%y"),20, legend = c("test") )

## The same thing in ggplot2
dat.cohort <- melt (dat.nest.wide,id.vars = "Survey.date", 
                  variable.name = "State.of.nest", value.name = "Number.of.nests")

dat.cohort <- subset(dat.cohort, 
                     dat.cohort$State.of.nest != "Black chicks" & 
                       dat.cohort$State.of.nest != "Standing chicks" & 
                       dat.cohort$State.of.nest != "Nest building")
ggplot(dat.cohort, aes(x = Survey.date, y = Number.of.nests, color = State.of.nest)) +
  theme_gray(base_size = 12) +
  geom_line(size = 0.8) +
  geom_point(size = 2) + 
  scale_color_manual(values = c("forest green", "blue", "red", "black", "darkblue"), 
                     name = "State of the nest", 
                     labels = c("Fledglings", "Incubation", "Abandoned", "Total nests", "Nestlings")) +
  labs(x = "Month", y = "Number of Nests", size = 10) 

## Other exploratory analysis
