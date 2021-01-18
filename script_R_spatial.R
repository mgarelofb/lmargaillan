
## some packages
library(sp)
library(raster)
library(rgeos)
library(rgdal)
library(raster)
library(adehabitatHS)
library(adehabitatHR)
library(maptools)



## data
load("/home/mgarel/Mathieu/Travail/Collaborations/Mov-it/BDD/output/all_gps_data_uos.Rdata")
data <- subset(data , site%in% "BouBel")
length(unique(data$cap_id)) # en théorie 40 animaux/années

data$mon <- as.numeric(substr(data$loc_date , 6, 7))
                                        # add a column with month
data$id <- 1 # fake

data <- subset(data , loc_validated_manu %in% "normal" | is.na(loc_validated_manu)) # cleaning
data <- subset(data , loc_outlier1 %in% FALSE) # bis


data.nona <- subset(data, !is.na(loc_x_lb93)) # create a dataset without NA
coordinates(data.nona) <- c("loc_x_lb93" , "loc_y_lb93")
proj4string(data.nona) <- CRS("+init=epsg:2154")
                                        # Note spatial obejct -> coordinates are not
                                        # allowed to contain missing values

area <- mcp(data.nona["id"] , percent = 100) # a mcp of the study area based on GPS locations
area <- gBuffer(area , width = 2000) # + 2000m


## map
mnt <- raster("/home/mgarel/Mathieu/DATA_GIS/Belledonne/DEPT38.asc")
proj4string(mnt) <- CRS("+init=epsg:2154")

mnt <- crop(mnt, area)
mnt <- mask(mnt, area)
                                        # crop before mask (less time consuming)

plot(mnt)
plot(area , add = T , lwd = 4) # OK


slope <- terrain(mnt , opt = "slope" , unit = "degrees")


## some analyse of habitat use (slope) by ibex (used vs available) /month (at the population level)

res <- cbind.data.frame(mon = 1:12 , slope.used = NA , slope.available =  NA) # results

for(i in 1:12){# a loop/month

    ## compute slope available for ibex within a given month (at the population level)
    available <- mcp(data.nona[data.nona$mon == i,]["id"] , percent = 100)
                                        # we compute the mcp from all gps locations (ie from all
                                        # ibex) for defining habitat available for ibex a given month
    
    available <- mask(slope , available)
    res$slope.available[res$mon == i] <- mean(values(available) , na.rm = T) # compute average slope (slope is equal to NA at the border of the mnt -> require to add na.rm = T)

    ## compute slope used by ibex within a given month
    res$slope.used[res$mon == i] <- mean(extract(slope , data.nona[data.nona$mon == i,]),na.rm = T)

    cat(i , "\n")    
    
    }

par(mar = c(5,5,1,1))
plot(res$mon, res$slope.used , ylim = c(10,55) , type = "h" , lwd = 15 , col = "grey80" , lend = 3 , xlim = c(0.5,12.5) , xlab = "Months" , ylab = "Average slope" , cex.lab = 1.5)
lines(res$mon+0.5, res$slope.available , type = "h" , lwd = 15 , col = "grey40" , lend = 3)
legend(bty = "n" , 0, 55 , col = c("grey80","grey40") , pch = c(16,16), legend = c("slope used by ibex","slope available for ibex"))
                                        # selection for steep slopes during
                                        # winter (steep slopes = snow free); selection much less marked
                                        # during summer (steep slopes in summer = rocks without
                                        # food?)  warning: we mixed males/females!


######
## end
