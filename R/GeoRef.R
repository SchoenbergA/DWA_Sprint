### GeoRef DWA Pfalz

# set environment
require(rgdal)
require(raster)
path <- "C:/Envimaster/DWA_Sprint/GeoRef/Data_GeoRef/"
# load data
pfz <- read.xlsx(file.path(path,"pfalz_places.xlsx"))
wen <- readOGR(file.path(path,"Wenker_places_wgs.shp"))
msc <- readOGR(file.path(path,"pfalz_mask.shp"))
all <- read.csv("C:/Envimaster/DWA_Sprint/R/alle-gid-orte.csv",sep="")
head(wen)
colnames(all)[2] <-"ort"
colnames(all)[6] <-"GID"

# add geometry to wenker
geo <- geom(wen)
# write UTM geometry
wen$x <- geo[,2]
wen$y <- geo[,3]

# crop

wen_pfz <- crop(wen,msc)
plot(wen_pfz)

# simple GeoRef function

DWA_GeoRef <- function(pfz,wen){
  
  # add columns
  pfz$x <- 999
  pfz$y <- 999
  pfz$GID <-999
  pfz$res <-999
  # loop for each row in Pfalz Data
  for(i in 1:nrow(pfz)){
    # check if ortsname pfz is in tab_crdkerorte
    if(pfz$Ort[i]%in%wen$ort==T){
      
      if(length(which(pfz$Ort==pfz$Ort[i]))>1){
        pfz$res[i] <- "not an unique name"
      } else {
        if(length(which(wen$ort==pfz$Ort[i]))>1){
          #cat(paste0("multiple entries for ",pfz$Ort[i]," detected. Skipping",sep="\n"))
          pfz$res[i] <- "multiple entires detected"
        } else {
          # write coords from wenker to pfz
          pfz$res[i] <- "detected"
          pfz$x[i] <- wen$x[which(wen$ort==pfz$Ort[i])]
          pfz$y[i] <- wen$y[which(wen$ort==pfz$Ort[i])]
          #pfz$GID[i] <- wen$gid[which(wen$ort==pfz$Ort[i])]
        }
      }
    } else {
      #cat(paste0("pfz place ",pfz$Ort[i], " not detected in tab_crdker Places",sep="\n"))
      pfz$res[i] <- "not detected"
    }
  }
  nobj <- nrow(pfz)
  # Print results
  cat(" ",sep="\n")
  cat("##############################################",sep="\n")
  cat(paste0("Total pfz places ",nobj),sep="\n")
  cat(paste0(length(which(pfz$res=="detected"))," detected (",round(length(which(pfz$res=="detected"))/nobj*100,digits = 2),"%)"),sep="\n")
  cat(paste0(length(which(pfz$res=="multiple entires detected"))," multiple entires detected (",round(length(which(pfz$res=="multiple entires detected"))/nobj*100,digits = 2),"%)"),sep="\n")
  cat(paste0(length(which(pfz$res=="not an unique name"))," not an unique name (",round(length(which(pfz$res=="not an unique name"))/nobj*100,digits = 2),"%)"),sep="\n")
  cat(paste0(length(which(pfz$res=="not detected"))," not detected (",round(length(which(pfz$res=="not detected"))/nobj*100,digits = 2),"%)"),sep="\n")
  
  # return
  return(pfz)
}# end of function

test1 <-DWA_GeoRef(pfz,wen)
test3 <- DWA_GeoRef(pfz,all)
test2 <-DWA_GeoRef(pfz,wen_pfz)

plot(test1)
head(test1)
test1_na <- na.omit(test3[,12:13])
plot(test1_na)
test2 <- SpatialPointsDataFrame(test3[12:13],test3)
crs(test2)<-"+proj=longlat +datum=WGS84 +no_defs"
plot(test2)
require(mapview)
mapview(test2)+msc
