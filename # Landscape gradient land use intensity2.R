### Complex landscape gradient ABI Land use intensity

library(raster)
library(rgdal)
#library(gdalUtils)
library(sf)
#library(purrr)
#library(landscapemetrics)
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
#plot(dem)

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
plot(n50_adm_area)

e <- extent(n50_adm_area)
dem_trd <- crop(dem, e) # making a new raster; the cropped dem
plot(dem_trd)
plot(n50_adm_area, add=TRUE)

#Begge komponenter blir beregnet som frekvenser i standardraster 
#(figur 6) og 500 m målenabolag: 
# ByI på grunnlag av forekomst av
#primærvariabelen 

############
### PREPARING NA-RASTER FOR CALCULATIONS
############
target_area <- rasterize(n50_adm_area, dem_trd, field = 1, background=NA)
plot(target_area)

#rcl <- matrix(c(0, 2, NA, 2, 100, 1), ncol = 3, byrow = TRUE)
#target_area_raster <-  reclassify(target_area_raster, rcl = rcl)
#plot(na_raster)


n50_lc_a <- readOGR(dsn=fgdb,layer= "N50_Arealdekke_omrade")  
unique(n50_lc_a$objtype)
n50_lc_water <- n50_lc_a [n50_lc_a$objtype== "ElvBekk" | 
                                          n50_lc_a$objtype== "Havflate" | 
                                          n50_lc_a$objtype== "InnsjÃ¸" | 
                                          n50_lc_a$objtype== "InnsjÃ¸Regulert" ,]


plot(dem_trd)
plot(n50_lc_water, col = "blue", border="blue", add=TRUE)
#Rasterize Kfl_transportaion (#roads, railroads)
na_raster <- rasterize(n50_lc_water, dem_trd, field = 5, background=1)
plot(na_raster)
na_raster <- na_raster*target_area
plot(na_raster)

# Reclassify
# Here, we assign the raster values in the ranges 0-0., 0.1-100 
# are reclassified to take values 1 and NA respectively.
rcl <- matrix(c(0, 2, 1, 2, 100, NA), ncol = 3, byrow = TRUE)
na_raster <-  reclassify(na_raster, rcl = rcl)
plot(na_raster)

# Calculating correction raster for LUI
fwModel <- focalWeight(na_raster, 500, type='circle')
fwModel[fwModel>0] <- 1
na_focal<-focal(na_raster, w=fwModel ,fun=sum , na.rm=TRUE)
plot(na_focal)

############
### byl
############

#bygninger (av ethvert slag) i GABregistreret
#forekomst av ett eller flere av linjeelementene fra datasettene
#N50 anlegg (inkludert fremtredende kraftlinjer) og
#N50 samferdsel (bane og veg senterlinje, traktorveg og sti ikke #inkludert).

### Building building data from Matrikkelen

fgdb <- "C:/Users/trosim/Documents/GIS_Trond/Land_Use_Intensity/Basisdata_5001_Trondheim_25833_MatrikkelenBygning_FGDB.gdb"

# List all feature classes in a file geodatabase
subset(ogrDrivers(), grepl("GDB", name))
fc_list <- ogrListLayers(fgdb)
print(fc_list)

building_GAB <- readOGR(dsn=fgdb,layer= "matrikkelenbygning_bygning")
plot(building_GAB, pch=18, cex= 0.1, col = "red", add=TRUE)
#mapview::mapview(building_GAB) # zooming in on data
byl_buildings <- rasterize(building_GAB, dem_trd, field = 1, background=0)
plot(byl_buildings)

# The input file geodatabase
fgdb <- "C:/Users/trosim/Documents/GIS_Trond/Land_Use_Intensity/Basisdata_5001_Trondheim_25833_N50Kartdata_FGDB.gdb"

#Facilities - points
n50_buildings_facilities_position <- readOGR(dsn=fgdb,layer= "N50_BygningerOgAnlegg_posisjon") 
unique(n50_buildings_facilities_position$objtype)

n50_facilities <- n50_buildings_facilities_position [n50_buildings_facilities_position$objtype== "Hoppbakke" | 
                                                n50_buildings_facilities_position$objtype== "MastTele" |
                                                n50_buildings_facilities_position$objtype== "SpesiellDetalj" | 
                                                n50_buildings_facilities_position$objtype== "Tank" | 
                                                n50_buildings_facilities_position$objtype== "TÃ¥rn" | 
                                                n50_buildings_facilities_position$objtype== "Vindkraftverk" ,]

plot(n50_facilities, pch=18, cex=0.5, col="blue", add=TRUE)
byl_facilities <- rasterize(n50_facilities, dem_trd, field = 1, background=0)
plot(byl_facilities)

## transportation line data
n50_transport <- readOGR(dsn=fgdb,layer= "N50_Samferdsel_senterlinje")        
unique(n50_transport$objtype)
veg_bane <- n50_transport [n50_transport$objtype== "Bane" | n50_transport$objtype== "VegSenterlinje" ,]
plot(dem_trd)
plot(veg_bane, add=TRUE)
#mapview::mapview(n50_veg_bane) # zooming in on data
#Rasterize Kfl_transportaion (#roads, railroads)
byl_transport <- rasterize(veg_bane, dem_trd, field = 1, background=0)
plot(byl_transport)

# Facilities - lines (e.g. power lines)
n50_buildings_facilities_position_cl <- readOGR(dsn=fgdb,layer= "N50_BygningerOgAnlegg_senterlinje") 
unique(n50_buildings_facilities_position_cl$objtype, add=TRUE)
falilities_lines <- n50_buildings_facilities_position_cl [n50_buildings_facilities_position_cl$objtype== "LuftledningLH",]
plot(dem_trd)
plot(falilities_lines, col="red", add=TRUE)
#Rasterize Kfl_lines (#e.g. power lines)
byl_facilities_lines <- rasterize(falilities_lines, dem_trd, field = 1, background=0)
plot(byl_facilities_lines, add=TRUE)

# Adding rasters
byl_raw <- byl_buildings + byl_facilities + byl_transport + byl_facilities_lines
plot(byl_raw)

# Reclassify
# Here, we assign the raster values in the ranges 0-0., 0.1-100 
# are reclassified to take values 0 and 1 respectively.
rcl <- matrix(c(0, 0.1, 0, 0.1, 100, 1), ncol = 3, byrow = TRUE)
byl_raw_recl = reclassify(byl_raw, rcl = rcl)
plot(byl_raw_recl)
byl_corrected <- byl_raw_recl*na_raster
plot(byl_corrected)

par(mfrow=c(1,2))

# Calculating focal statistics
fwModel <- focalWeight(byl_corrected, 500, type='circle')
fwModel[fwModel>0] <- 1
byl_focal<-focal(byl_corrected, w=fwModel ,fun=sum , na.rm=TRUE)
plot(byl_focal)
byl <- byl_focal/na_focal
#byl <- byl_focal*na_raster
plot(byl)

############
### Kfl
############

#KfI på grunnlag av forekomst av en eller flere av #arealkategoriene
#som indikerer konstruert fastmark (datasett: N50): 
# bebygd #areal: 
#o tettbebygd areal, 
#o industriområde, 
#o lufthavn, 
#o steinbrudd, 
##gravplass, 
#o sport/idrettsanlegg 
# (inkl. alpinbakke, hoppbakke og golfbane)

#Built up areas as landcover
n50_lc_a <- readOGR(dsn=fgdb,layer= "N50_Arealdekke_omrade")  
unique(n50_lc_a$objtype)
n50_lc_build_up_area <- n50_lc_a [n50_lc_a$objtype== "Alpinbakke" | 
                                        n50_lc_a$objtype== "BymessigBebyggelse" | 
                                        n50_lc_a$objtype== "Gravplass" | 
                                        n50_lc_a$objtype== "Golfbane" | 
                                        n50_lc_a$objtype== "IndustriomrÃ¥de" | 
                                        n50_lc_a$objtype== "SportIdrettPlass" | 
                                        n50_lc_a$objtype== "Steinbrudd" | 
                                        n50_lc_a$objtype== "Tettbebyggelse" ,]

plot(dem_trd)
plot(n50_lc_build_up_area, col="grey", border="grey31", add=TRUE)

#Rasterize
kfl_built_up_area <- rasterize(n50_lc_build_up_area, dem_trd, field = 1, background=0)
plot(kfl_built_up_area)

#Built up areas from buildings and facilities dataset (not buildings/facilities that are already inclued in byl)
unique(n50_buildings_facilities_position$objtype)
n50_lc_build_up_areas_2 <- n50_buildings_facilities_position [n50_buildings_facilities_position$objtype== "Campingplass" | 
                                                n50_buildings_facilities_position$objtype== "ParkeringsomrÃ¥de" ,]

plot(n50_lc_build_up_areas_2, pch=18, cex=1, col="blue", add=TRUE)
kfl_built_up_areas_2 <- rasterize(n50_lc_build_up_areas_2, dem_trd, field = 1, background=0)
plot(kfl_built_up_areas_2)

kfl_raw <- kfl_built_up_area + kfl_built_up_areas_2
plot(kfl_raw)

# Reclassify
kfl_raw_recl = reclassify(kfl_raw, rcl = rcl)
plot(kfl_raw_recl)

kfl_corrected <- kfl_raw*na_raster

# Calculating focal statistics
fwModel <- focalWeight(kfl_corrected, 500, type='circle')
fwModel[fwModel>0] <- 1
kfl_focal<-focal(kfl_corrected, w=fwModel ,fun=sum , na.rm=TRUE)
plot(kfl_focal)
#kfl <- kfl_focal*na_raster
kfl <- kfl_focal/na_focal
plot(kfl)

#######################################
### CALCULATING THE INDEX  ############
#######################################


# Weighing the different parts of the index
ABI <- 2*log2(4+81*byl)+log2(4+81*kfl)-6

ABI
plot(ABI)

plot(ABI, col = rev(brewer.pal(9, "RdYlGn")), box = F, axes = F, legend = F)
ABI <- ABI*na_raster



#Fancy plot
plot(ABI, col = rev(brewer.pal(9, "RdYlGn")), box = F, axes = F, legend = F, add=TRUE)
plot(ABI, legend.only = T, horizontal = T, add = T, col = rev(brewer.pal(9, "RdYlGn")), smallplot = c(0.25, 0.75, 0.08, 0.1))
title(sub = "Land use intensity")
plot(n50_adm_area, lty = 5, lwd = 2, border = "snow", add=TRUE)


# Calculate slope:
slope <- terrain(dem_trd, opt="slope")
plot(slope)

# Calculate aspect
aspect <- terrain(dem_trd, opt = "aspect")
plot(aspect)

# Calculate hillshade
hillshade <- hillShade(slope, aspect, 30, 315)
plot(hillshade)
plot(hillshade, col=grey(0:100/100), legend=FALSE, axes=TRUE, add=TRUE)

#Complex cartography
plot(hillshade, col=grey(0:100/100), box = F, legend=FALSE, axes=F)
plot(veg_bane, col= "grey41", add=TRUE)

plot(facilities, pch=18, cex=0.3, col= "grey31", add=TRUE)
plot(n50_buildings_facilities_position_cl, col= "grey31", add=TRUE)
plot(veg_bane, col= "grey31", add=TRUE)
plot(building_GAB, pch=18, cex= 0.1, col = "grey0", add=TRUE)

#for cartography (later)
water_raster <- rasterize(n50_lc_water, dem_trd, field = 1, background=NA)
#plot(water_raster, col="grey", box=F, axes = F, legend = F, alpha=.5)
#box = F, axes = F, legend = F, alpha=.5, add=T

plot(water_raster, col = rev(brewer.pal(1, "Blues")), box = F, axes = F, legend = F, alpha=.5, add=T)

plot(ABI, col = rev(brewer.pal(9, "RdYlGn")), box = F, axes = F, legend = F, alpha=.5, add=T)

plot(n50_adm_area, lty = 5, lwd = 2, border = "snow", add=TRUE)

n50_adm_area <- readOGR(dsn=fgdb,layer= "N50_AdministrativeOmråder_omrade")  
plot(n50_adm_area, add=TRUE)

plot(ABI, legend.only = T, col = rev(brewer.pal(9, "RdYlGn")))
#plot(ABI, legend.only = T, horizontal = T, add = T, col = rev(brewer.pal(9, "RdYlGn")), smallplot = c(0.35, 0.65, 0.18, 0.2))
title(main = "Land use intensity")

hist(ABI,
     main = "Land use intensity",
     xlab = "Land use indtensity index", ylab = "Frequency",
     col = "springgreen")

ABI
unique(ABI)

mapview::mapview(ABI) # zooming in on data


### Comparing with original gradient
CLGai  <- raster("C:/Users/trosim/Documents/GIS_Trond/Land_Use_Intensity/CLG/CLGai.tif")
#plot(CLGai)
e <- extent(ABI)
clg_ai_trd <- crop(CLGai, e) # making a new raster; the cropped dem
plot(clg_ai_trd)

rasterOptions(tolerance = 0.5)

clg_ai_trd <- clg_ai_trd*na_raster

par(mfrow=c(1,2))
plot(ABI, col = rev(brewer.pal(9, "RdYlGn")), box = F, axes = F, legend = F)
plot(clg_ai_trd, col = rev(brewer.pal(9, "RdYlGn")), box = F, axes = F, legend = F)

cor(values(ABI),
    values(clg_ai_trd),
    use = "na.or.complete")

residuals <- clg_ai_trd - ABI
plot(residuals, col = rev(brewer.pal(5, "RdYlBu")), box = F, axes = F, legend = T)

res <- abs(residuals)
plot(res, col = rev(brewer.pal(5, "RdYlBu")), box = F, axes = F, legend = T)


?mapview
mapView(ABI, map = NULL,
        maxpixels = mapviewGetOption("mapview.maxpixels"),
        col.regions = mapviewGetOption("raster.palette")(256), at = NULL,
        na.color = mapviewGetOption("na.color"), use.layer.names = FALSE,
        values = NULL, map.types = mapviewGetOption("basemaps"),
        alpha.regions = 0.7, legend = mapviewGetOption("legend"),
        legend.opacity = 1, trim = TRUE)

mapView(clg_ai_trd, map = NULL,
        maxpixels = mapviewGetOption("mapview.maxpixels"),
        col.regions = mapviewGetOption("raster.palette")(256), at = NULL,
        na.color = mapviewGetOption("na.color"), use.layer.names = FALSE,
        values = NULL, map.types = mapviewGetOption("basemaps"),
        alpha.regions = 0.7, legend = mapviewGetOption("legend"),
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

#n50_rest_a <- readOGR(dsn=fgdb,layer= "N50_Restriksjonsområder_omrade")  
#n50_rest_l <- readOGR(dsn=fgdb,layer= "N50_Restriksjonsområder_grense")    

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

plot(hillshade, col=grey(0:100/100), box = F, legend=FALSE, axes=F)
plot(veg_bane, col= "grey41", add=TRUE)
#plot(byl_facilities, pch=18, cex=0.3, col= "grey31", add=TRUE)
plot(n50_buildings_facilities_position_cl, col= "grey31", add=TRUE)
plot(veg_bane, col= "grey31", add=TRUE)
plot(building_GAB, pch=18, cex= 0.1, col = "grey0", add=TRUE)
plot(water_raster, col = rev(brewer.pal(1, "Blues")), box = F, axes = F, legend = F, alpha=.7, add=T)
plot(ABI, col = rev(brewer.pal(9, "RdYlGn")), box = F, axes = F, legend = T, alpha=.7, add=T)
#plot(n50_adm_area, lty = 5, lwd = 2, border = "snow", add=TRUE)
plot(ABI, legend.only = T, add = T, col = rev(brewer.pal(9, "RdYlGn")))

addnortharrow(pos="bottomleft", padin = c(0.15, 0.45), scale=0.5)
addscalebar(plotunit = "m", widthhint = 0.25,
            unitcategory = "metric", htin = 0.1, padin = c(0.15, 0.15),
            style = "ticks", bar.cols = c("black", "white"), lwd = 1,
            linecol = "black", tick.cex = 0.7, labelpadin = 0.08, label.cex = 0.8,
            label.col = "black", pos = "bottomleft")

