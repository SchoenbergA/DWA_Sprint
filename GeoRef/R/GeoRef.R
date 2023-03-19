### DWA GeoRef 

# set environment
require(rgdal)
require(raster)
require(geojsonR)
require(openxlsx)
require(mapview)
require(rgeos)

path <- "C:/Envimaster/DWA_Sprint/GeoRef/Data_GeoRef/"
source("C:/Envimaster/DWA_Sprint/GeoRef/R/GeoRef_function.R")
source("C:/Envimaster/DWA_Sprint/GeoRef/R/GeoRef_function2.R")
# load data
pfz <- read.xlsx(file.path(path,"pfalz_places.xlsx"))
msc <- readOGR(file.path(path,"pfalz_mask.shp"))
grd <- readOGR(file.path(path,"DWA_Grid_clean_wgs.shp"))
all <- read.csv(file.path(path,"alle-gid-orte.csv"),sep="",encoding = "UTF-8")

# solve several HiWi issues
which(pfz$Buchstabe==999)
which(pfz$Nummer==999)
pfz <- pfz[pfz$Buchstabe!=999,]
pfz <- pfz[pfz$Nummer!=999,]

which(pfz$Buchstabe=="Z'")
pfz[which(pfz$Buchstabe=="Z'"),]

pfz$Buchstabe[which(pfz$Buchstabe=="Z'")]<-"z'"

which(pfz$Buchstabe=="Z´")
pfz[which(pfz$Buchstabe=="Z´"),]

pfz$Buchstabe[which(pfz$Buchstabe=="Z´")]<-"z'"

which(pfz$Buchstabe=="f´")
pfz[which(pfz$Buchstabe=="f´"),]

pfz$Buchstabe[which(pfz$Buchstabe=="f´")]<-"f'"

which(pfz$Nummer==1)
pfz[pfz$Nummer==1,]
pfz$Nummer[pfz$Nummer==1]<-2

# run GeoRef ###################################################################

test0 <- DWA_GeoRef(dwa=pfz,grd=grd,dat=all,pos_x=4,pos_y=5,buffer=NULL)
test1 <- DWA_GeoRef(dwa=pfz,grd=grd,dat=all,pos_x=4,pos_y=5,buffer = 1000)
test2 <- DWA_GeoRef(dwa=pfz,grd=grd,dat=all,pos_x=4,pos_y=5,buffer = 2000)
test3 <- DWA_GeoRef(dwa=pfz,grd=grd,dat=all,pos_x=4,pos_y=5,buffer = 3000)
test5 <- DWA_GeoRef(dwa=pfz,grd=grd,dat=all,pos_x=4,pos_y=5,buffer = 5000)

# test threshold for similarity

th90<- DWA_GeoRef2(dwa=pfz,grd=grd,dat=all,pos_x=4,pos_y=5,buffer = 2000,th = 0.9)
th80<- DWA_GeoRef2(dwa=pfz,grd=grd,dat=all,pos_x=4,pos_y=5,buffer = 2000,th = 0.8)
th50<- DWA_GeoRef2(dwa=pfz,grd=grd,dat=all,pos_x=4,pos_y=5,buffer = 2000,th = 0.5)

# check for dubplicated GID
pfz_2000 <- test2[which(test2$GeoRef=="detected"),]
duplicated(pfz_2000$GID)

# write
# write.xlsx(pfz_2000,file.path(path,"Pfalz_GeoRef_Buf_2000.xlsx"),overwrite = T)

# Compare buffer results
which(test0$GID%in%test2$GID)
which(!test2$GID%in%test0$GID)

# view place undetected without buffer
test2[72,]
all[which(all$name=="Enchenberg"),]

### view buffer effect
mapview(grd[grd$name=="11h'",])
msc <- grd[grd$name=="11h'",]
msc <-sp::spTransform(msc, CRSobj = "+proj=moll")
msc2 <-rgeos::gBuffer(spgeom = msc, width = 2000)
dat_spt <- sp::SpatialPointsDataFrame(all[,4:5],all)
sp::proj4string(dat_spt) <- "+proj=longlat +datum=WGS84 +no_defs"
dat_spt <-sp::spTransform(dat_spt, CRSobj = "+proj=moll")
dat_grd <- crop(dat_spt,msc)
dat_grd2 <- crop(dat_spt,msc2)

mapview(dat_grd2)+grd[grd$name=="11h'",]+msc2

# weitere ideen
require(stringdist)
stringsim("Marburg","Mariburg","osa")
stringsim("Marburg","Mariburg","lv")