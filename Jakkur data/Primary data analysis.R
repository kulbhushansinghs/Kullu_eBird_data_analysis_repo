#Libraries
require(dplyr)
require(tidyr)
require(reshape2)

setwd("/Users/Kullu/Dropbox (Snow Leopard Trust)/Kullu_desktop/Git/Kullu_eBird_data_analysis_repo/Jakkur data")
dat.nest <- read.csv("Jukkur painted Stork nest data.csv")
dat.pop <- read.csv("Jukkur painted Stork population data.csv")

dat.nest$Survey.date <- as.Date(dat.nest$Survey.date, format = "%d/%m/%Y")
dat.pop$Date <- as.Date(dat.pop$Date, format = "%d/%m/%y")
plot(dat.pop$Date, dat.pop$Number.of.nests, 
     lwd = 2, type = "l", ylim = c(0,210),
     xlab = "Month", ylab = "Numbers")
points(dat.pop$Date, dat.pop$Number.of.nests,cex = 1, pch = 16)
points(dat.pop$Date, dat.pop$Number.of.chicks, 
       lwd = 2, type = "l", lty = 2)
points(dat.pop$Date, dat.pop$Number.of.chicks, pch = 16)
points(dat.pop$Date, dat.pop$Number.of.adults, lwd = 2, type = "l", lty = 3, col = "red")
points(dat.pop$Date, dat.pop$Number.of.adults, pch = 16, col = "red")
dat.nest.wide <- dcast(dat.nest, formula = Nest.Id ~ Survey.date)

for(i in 1:50){
barplot(as.numeric(dat.nest.wide[i,2:15]))
}
