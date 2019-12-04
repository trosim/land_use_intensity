### Complex landscape gradient ABI Land use intensity

library(raster)
library(rgdal)
library(gdalUtils)
library(sf)
library(purrr)
library(landscapemetrics)
library(mapview)
library(RColorBrewer)

#library(rasterVis)

### Preparations
rm(list=ls()) ### remove data frames and object from part I
memory.limit(size=357500) #increasing memory limit for faster processing

setwd("~/GIS_Trond/Land_Use_Intensity")

DEM100 <- raster("DEM100.tif") #Master DEM (keep this file without changes)
dem <- DEM100

#checking raster
extent(dem)
plot(dem)

### N50 DATA

# Reading in from ESRI geodatabase:
# Library (rgdal)
# The input file geodatabase
fgdb <- "C:/Users/trosim/Documents/GIS_Trond/Land_Use_Intensity/Basisdata_5001_Trondheim_25833_N50Kartdata_FGDB.gdb"
# List all feature classes in a file geodatabase
subset(ogrDrivers(), grepl("GDB", name))
fc_list <- ogrListLayers(fgdb)
print(fc_list)

# Municipality border
n50_adm_area <- readOGR(dsn=fgdb,layer= "N50_AdministrativeOmråder_omrade")  
plot(dem_trd)
plot(n50_adm_area, add=TRUE)

extent(dem)
e <- extent(n50_adm_area)
dem_trd <- crop(dem, e) # making a new raster; the cropped dem
plot(dem_trd)
plot(n50_adm_area, add=TRUE)

### N50 Anlegg

#n50_rest_a <- readOGR(dsn=fgdb,layer= "N50_Restriksjonsområder_omrade")  
#n50_rest_l <- readOGR(dsn=fgdb,layer= "N50_Restriksjonsområder_grense")    

# land cover
n50_lc_a <- readOGR(dsn=fgdb,layer= "N50_Arealdekke_omrade")  
plot(dem_trd)
plot(n50_lc_a, add=TRUE)

unique(n50_lc_a$objtype)
n50_lc_build_infra <- n50_lc_a [n50_lc_a$objtype== "Alpinbakke" | 
                                  n50_lc_a$objtype== "BymessigBebyggelse" | 
                                  n50_lc_a$objtype== "Gravplass" | 
                                  n50_lc_a$objtype== "Golfbane" | 
                                  n50_lc_a$objtype== "IndustriomrÃ¥de" | 
                                  n50_lc_a$objtype== "SportIdrettPlass" | 
                                  n50_lc_a$objtype== "Steinbrudd" | 
                                  n50_lc_a$objtype== "Tettbebyggelse" ,]
plot(dem_trd)
plot(n50_lc_build_infra, add=TRUE)

# transportation
n50_transport <- readOGR(dsn=fgdb,layer= "N50_Samferdsel_senterlinje")        
plot(n50_transport, add=TRUE)
unique(n50_transport$objtype)
n50_veg_bane <- n50_transport [n50_transport$objtype== "Bane" | n50_transport$objtype== "VegSenterlinje" ,]
plot(dem_trd)
plot(n50_veg_bane, add=TRUE)
#mapview::mapview(n50_veg_bane) # zooming in on data

#Built up areas as landcover
plot(dem_trd)
plot(n50_lc_build_infra, col="grey", add=TRUE)

# Facilities (and buildings) from N50
#Buildings # alternative to GAB, not in use here
#n50_buildings_facilities <- readOGR(dsn=fgdb,layer= "N50_BygningerOgAnlegg_omrade")     
#plot(n50_buildings_facilities, col="red", add=TRUE)
#unique(n50_buildings_facilities$objtype)

#Facilities - points
n50_buildings_facilities_position <- readOGR(dsn=fgdb,layer= "N50_BygningerOgAnlegg_posisjon") 
unique(n50_buildings_facilities_position$objtype)
facilities <- n50_buildings_facilities_position
plot(facilities, pch=18, cex=0.3, col="red", add=TRUE)

# Facilities - lines
n50_buildings_facilities_position_cl <- readOGR(dsn=fgdb,layer= "N50_BygningerOgAnlegg_senterlinje") 
unique(n50_buildings_facilities_position_cl$objtype, add=TRUE)
plot(dem_trd)
plot(n50_buildings_facilities_position_cl, add=TRUE)

###Rasterize (kan flyttes opp i scriptet til hvert enkelt deltema)

# Rasterize landcover for built-up areas
Kfl_landcover <- rasterize(n50_lc_build_infra, dem_trd, field = 1, background=0)
plot(Kfl_landcover, add=TRUE)

#Rasterize Kfl_transportaion (#roads, railroads)
Kfl_transportation <- rasterize(n50_veg_bane, dem_trd, field = 1, background=0)
plot(Kfl_transportation)
Kfl_transportation
cellStats(Kfl_transportation, range)

#Rasterize Kfl_lines (#e.g. power lines)
Kfl_facilities_lines <- rasterize(n50_buildings_facilities_position_cl, dem_trd, field = 1, background=0)
plot(Kfl_facilities_lines, add=TRUE)

#Rasterize other facilities (e.g. ...)
Kfl_facilities <- rasterize(n50_buildings_facilities_position, dem_trd, field = 1, background=0)
plot(Kfl_facilities, add=TRUE)

# Adding rasters
Kfl_raw <- Kfl_landcover + Kfl_transportation + Kfl_facilities_lines + Kfl_facilities
plot(Kfl_raw)

# Reclassify
# Here, we assign the raster values in the ranges 0-0., 0.1-100 
# are reclassified to take values 0 and 1 respectively.
rcl <- matrix(c(0, 0.1, 0, 0.1, 100, 1), ncol = 3, byrow = TRUE)
Kfl_raw_recl = reclassify(Kfl_raw, rcl = rcl)
plot(Kfl_raw_recl)

# Calculating focal statistics
fwModel <- focalWeight(Kfl_raw_recl, 500, type='circle')
fwModel[fwModel>0] <- 1
Kfl_focal<-focal(Kfl_raw_recl, w=fwModel ,fun=sum , na.rm=TRUE)
plot(Kfl_focal)
Kfl_focal
Kfl <- Kfl_focal

### Building building data from Matrikkelen

fgdb <- "C:/Users/trosim/Documents/GIS_Trond/Land_Use_Intensity/Basisdata_5001_Trondheim_25833_MatrikkelenBygning_FGDB.gdb"

# List all feature classes in a file geodatabase
subset(ogrDrivers(), grepl("GDB", name))
fc_list <- ogrListLayers(fgdb)
print(fc_list)

building_GAB <- readOGR(dsn=fgdb,layer= "matrikkelenbygning_bygning")
plot(building_GAB, pch=18, cex= 0.1, col = "red", add=TRUE)
#mapview::mapview(building_GAB) # zooming in on data

#checking coordinate systems
st_crs(dem) #> Coordinate Reference System: NA
st_crs(building_GAB) #> Coordinate Reference System: NA
st_crs(n50_lc_a) #> Coordinate Reference System: NA

#First, we create a raster representing the presence or absence of buildings 
#(known as presence/absence rasters). In this case rasterize() requires only one argument 
#in addition to x and y (the aforementioned vector and raster objects): 
#a value to be transferred to all non-empty cells specified by field (results illustrated Figure #)).

buildings_pa <- rasterize(building_GAB, dem_trd, field = 1, background=0)
plot(buildings_pa)

#Then we perform focal statistics
fwModel <- focalWeight(buildings_pa, 500, type='circle')
fwModel[fwModel>0] <- 1
buildings_focal<-focal(buildings_pa, w=fwModel ,fun=sum , na.rm=TRUE)
plot(buildings_focal)
byl <- buildings_focal

#######################################
### CALCULATING THE INDEX  ############
#######################################


# Weighing the different parts of the index
ABI <- 2*log2(4+81*byl)+log2(4+81*Kfl)-6
plot(ABI, alpha=.5, add=T)

plot(ABI)

plot(ABI, col = rev(brewer.pal(9, "RdYlGn")), box = F, axes = F, legend = F)

######################################
### RESULTS ARE WRONG      ###########
### PROBABLY BECAUSE I    ############
### HAVE NOT CORRECTED FOR  ##########
### RATIOS (INSTEAD OF 81)  ##########
### TRY TO MASK OUT WATER  ###########
### PIXELS BEFORE CALC.! #############
######################################

#Fancy plot
plot(ABI, col = rev(brewer.pal(9, "RdYlGn")), box = F, axes = F, legend = F)
plot(ABI, legend.only = T, horizontal = T, add = T, col = rev(brewer.pal(9, "RdYlGn")), smallplot = c(0.25, 0.75, 0.08, 0.1))
par(lheight=.5, adj = 0.5)
par(lheight=.5, adj = 0.5)
title(sub = "Land use intensity")

# Calculate slope:
slope <- terrain(dem_trd, opt="slope")
plot(slope)

# Calculate aspect
aspect <- terrain(dem_trd, opt = "aspect")
plot(aspect)

# Calculate hillshade
hillshade <- hillShade(slope, aspect, 30, 315)
plot(hillshade)
plot(hillshade, col=grey(0:100/100), legend=FALSE, axes=F, add=TRUE)

#Complex cartography
plot(hillshade, col=grey(0:100/100), box = F, legend=FALSE, axes=F)
plot(n50_veg_bane, col= "grey41", add=TRUE)

plot(n50_lc_build_infra, col= "grey31", border = "grey31", add=TRUE)
plot(facilities, pch=18, cex=0.3, col= "grey31", add=TRUE)
plot(n50_buildings_facilities_position_cl, col= "grey31", add=TRUE)
plot(building_GAB, pch=18, cex= 0.1, col = "grey0", add=TRUE)

plot(ABI, col = rev(brewer.pal(9, "RdYlGn")), box = F, axes = F, legend = F, alpha=.5, add=T)

marine <- n50_lc_a [n50_lc_a$objtype == "Havflate", ]
plot(marine, col = "skyblue2", border="skyblue2", add=TRUE)

freshwater <- n50_lc_a [n50_lc_a$objtype == "InnsjÃ" | n50_lc_a$objtype == "InnsjÃ¸Regulert" , ]
plot(freshwater2, col = "skyblue2", border="skyblue2", add=TRUE)

unique(n50_lc_a$objtype)
elv_bekk <- n50_lc_a [n50_lc_a$objtype == "ElvBekk" ,]
freshwater1 <- n50_lc_a [n50_lc_a$objtype == "InnsjÃ¸" ,]
freshwater2 <- n50_lc_a [n50_lc_a$objtype =="InnsjÃ¸Regulert" ,]

plot(elv_bekk, col= "skyblue2", border="skyblue2", add=TRUE)
plot(freshwater1, col= "skyblue2", border="skyblue2", add=TRUE)
plot(freshwater2, col= "skyblue2", border="skyblue2", add=TRUE)

plot(n50_adm_area, lty = 5, lwd = 2, border = "snow", add=TRUE)

plot(ABI, legend.only = T, horizontal = T, add = T, col = rev(brewer.pal(9, "RdYlGn")), smallplot = c(0.25, 0.75, 0.08, 0.1))
par(lheight=.5, adj = 0.5)
par(lheight=.5, adj = 0.5)
title(sub = "Land use intensity")


hist(ABI,
     main = "Land use intensity",
     xlab = "Land use indtensity index", ylab = "Frequency",
     col = "springgreen")

ABI
unique(ABI)

mapview::mapview(ABI) # zooming in on data

?mapview
mapView(ABI, map = NULL,
        maxpixels = mapviewGetOption("mapview.maxpixels"),
        col.regions = mapviewGetOption("raster.palette")(256), at = NULL,
        na.color = mapviewGetOption("na.color"), use.layer.names = FALSE,
        values = NULL, map.types = mapviewGetOption("basemaps"),
        alpha.regions = 0.3, legend = mapviewGetOption("legend"),
        legend.opacity = 1, trim = TRUE)


buildings_focal <- focalWeight(buildings_pa, 500, 'circle')
plot(buildings_focal, add=TRUE)

bui_focal_model[buildings_focal>0] <- 1

focalWeight(x, d, type=c('circle', 'Gauss', 'rectangle'))

fwModel <- focalWeight(modell_soil, 25, type='circle')
fwModel[fwModel>0] <- 1
b_focal<-focal(buildings_pa, w=fwModel ,fun=sum , na.rm=TRUE)

buildings_focal

buildings_pa

fwModel <- focalWeight(buildings_pa, 500, type='circle')
fwModel[fwModel>0] <- 1
b_focal<-focal(buildings_pa, w=fwModel ,fun=sum , na.rm=TRUE)
plot(b_focal)


buildings_focal <- focal(buildings_pa, w = matrix(1, nrow = 5, ncol = 5), fun = sum)
plot(buildings_focal)


#########################################
### ECOLOGICAL
#########################################


skog <- n50_lc_a [n50_lc_a$objtype == "Skog", ]
plot(skog, add=TRUE)
plot(skog, col = "green", add=TRUE)



#n50_lc_l <- readOGR(dsn=fgdb,layer= "N50_Arealdekke_grense")             
#plot(n50_lc_l, add=TRUE)

#Elv og bekk
n50_lc_cl <- readOGR(dsn=fgdb,layer= "N50_Arealdekke_senterlinje") 
unique(n50_lc_cl$objtype)
plot(dem_trd)
plot(n50_lc_cl, col= "blue", add=TRUE)

#Height contour lines
n50_height_cl <- readOGR(dsn=fgdb,layer= "N50_Høyde_senterlinje") 
plot(dem_trd)
plot(n50_height_cl, col="brown", add=TRUE)

#Minor details
n50_c_position <- readOGR(dsn=fgdb,layer= "N50_Arealdekke_posisjon")
unique(n50_c_position$objtype)

n50_transport_position <- readOGR(dsn=fgdb,layer= "N50_Samferdsel_posisjon")    
unique(n50_transport_position$objtype)

n50_buildings_facilities_l <- readOGR(dsn=fgdb,layer= "N50_BygningerOgAnlegg_grense")     
unique(n50_buildings_facilities_l$objtype)

n50_height_position <- readOGR(dsn=fgdb,layer= "N50_Høyde_posisjon")                
unique(n50_height_position$objtype)

n50_names <- readOGR(dsn=fgdb,layer= "N50_Stedsnavn_tekstplassering")    
unique(n50_names$objtype)

unique(n50_lc_a$objtype)
plot(n50_lc_a)
