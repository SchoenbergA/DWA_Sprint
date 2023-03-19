#' @param dwa df - with dwa ort transliteartion
#' @param grd shp - DWA Grid (full German Reich extend)
#' @param dat df - Table with x,y, and geometry_id information. Colnames have to be exact!
#' @param pos_xy numeric - position of x,y data. Default = format in table "alle orte"
#' @param buffer numeric - buffer in meter for the grid cells. Default=2000
#' @param th numeric - threshold for similarity in place name to get a suggestion.
#' @details The function is used to perform an automated georef for DWA tables using "alle orte" table. Hardcoded variable and columns names!
# GeoRef function
DWA_GeoRef2 <- function(dwa=pfz,grd=grd,dat=all,pos_x=4,pos_y=5,buffer=2000,th=0.8){
  # add x,y,GID and result columns to dwa
  dwa$x <- 999
  dwa$y <- 999
  dwa$GID <- 999
  dwa$GeoRef <-999
  dwa$comment <-999
  dwa$n_suggested <-999
  dwa$suggested <-999
  
  # convert dat input to spatial obj
  dat_spt <- sp::SpatialPointsDataFrame(dat[,pos_x:pos_y],dat)
  sp::proj4string(dat_spt) <- "+proj=longlat +datum=WGS84 +no_defs"
  
  if(is.null(buffer)==F){
    dat_spt <-sp::spTransform(dat_spt, CRSobj = "+proj=moll")
  }
  # get column with grid name equal to grid shape
  dwa$grid <- paste0(dwa$Nummer,dwa$Buchstabe)
  
  # loop all grid square
  for(i in 1:length(unique(dwa$grid))){
    #print(i)
    #print(unique(dwa$grid)[i])
    # get grid mask
    msc <- grd[grd$name==unique(dwa$grid)[i],]
    if(is.null(buffer)==F){
      msc <-sp::spTransform(msc, CRSobj = "+proj=moll")
      msc <-rgeos::gBuffer(spgeom = msc, width = buffer)
    }
    # crop dat to mask
    dat_grd <- crop(dat_spt,msc)
    # subset dwa
    dwa_grd <- dwa[dwa$grid==unique(dwa$grid)[i],]
    
    #cat(paste0(nrow(dwa_grd), " DWA places in grid ",msc$name ),sep="\n")
    
    # loop p for all dwa places in grid square i
    for(p in 1:nrow(dwa_grd)){
      # check if dwa name p in dat
      if(dwa_grd$Ort[p]%in%dat_grd$name){
        
        # n places in data == DWA place
        n_dat <- length(which(dat_grd$name==dwa_grd$Ort[p]))
        # n places in DWA == DWA place
        n_dwa <- length(which(dwa_grd$Ort==dwa_grd$Ort[p]))
        
        lv <-length(unique(dwa_grd$Bogen_Nummer[which(dwa_grd$Ort==dwa_grd$Ort[p])]))
        
        # multiple matches in data
        if(n_dat>1){
          dwa$GeoRef[which(dwa$Digi_Index==dwa_grd$Digi_Index[p])] <- "multiple entries in dat = dwa ortsname"
          #cat("multiple entries in dat = dwa ortsname",sep = "\n")
        }
        
        if(n_dwa>1&lv!=1&n_dat==1){
          dwa$GeoRef[which(dwa$Digi_Index==dwa_grd$Digi_Index[p])] <- "multiple entries in dwa = dwa ortsname and not equal 'Bogennummer'"
          #cat("multiple entries in dwa = dwa ortsname and not equal 'Bogennummer'",sep = "\n")
        }
        
        if(n_dwa>1&lv==1&n_dat==1){
          dwa$GeoRef[which(dwa$Digi_Index==dwa_grd$Digi_Index[p])] <- "detected"
          
          dwa$x[which(dwa$Digi_Index==dwa_grd$Digi_Index[p])] <- dat_grd$x[which(dat_grd$name==dwa_grd$Ort[p])]
          dwa$y[which(dwa$Digi_Index==dwa_grd$Digi_Index[p])] <- dat_grd$y[which(dat_grd$name==dwa_grd$Ort[p])]
          dwa$GID[which(dwa$Digi_Index==dwa_grd$Digi_Index[p])] <- dat_grd$geometry_id[which(dat_grd$name==dwa_grd$Ort[p])]
          dwa$comment[which(dwa$Digi_Index==dwa_grd$Digi_Index[p])] <- "local variation in DWA"
          #cat("dec",sep = "\n")
        }
        
        if(n_dwa==1&n_dat==1){
          dwa$GeoRef[which(dwa$Digi_Index==dwa_grd$Digi_Index[p])] <- "detected"
          dwa$x[which(dwa$Digi_Index==dwa_grd$Digi_Index[p])] <- dat_grd$x[which(dat_grd$name==dwa_grd$Ort[p])]
          dwa$y[which(dwa$Digi_Index==dwa_grd$Digi_Index[p])] <- dat_grd$y[which(dat_grd$name==dwa_grd$Ort[p])]
          dwa$GID[which(dwa$Digi_Index==dwa_grd$Digi_Index[p])] <- dat_grd$geometry_id[which(dat_grd$name==dwa_grd$Ort[p])]
          dwa$comment[which(dwa$Digi_Index==dwa_grd$Digi_Index[p])] <- "exact 1 match in grid shape"
          #cat("dec",sep = "\n")
        }
        
        
      } else {
        dwa_grd$Ort[p]
        
        # get similartity of dwa place to all names in dat
        stm <-stringsimmatrix(dwa_grd$Ort[p],dat_grd$name)
       
        # save n similary items with lv>th to comment
        if(length(which(stm>th))>0){
        dwa$GeoRef[which(dwa$Digi_Index==dwa_grd$Digi_Index[p])] <- "suggested match detected"
        dwa$n_suggested[which(dwa$Digi_Index==dwa_grd$Digi_Index[p])] <- length(which(stm>th))
        dwa$suggested[which(dwa$Digi_Index==dwa_grd$Digi_Index[p])] <- paste0(dat_grd$name[which(stm>th)],collapse = ", ")
        } else {
        dwa$GeoRef[which(dwa$Digi_Index==dwa_grd$Digi_Index[p])] <- "not detected"
        #cat("notdec",sep = "\n")
        }
      }
    } # end loop p
    
  }# end loop i for grid square
  
  # print results
  cat(" ",sep="\n")
  
  cat(paste0(nrow(dwa)," places"),sep="\n")
  cat(paste0(length(which(dwa$GeoRef=="detected")), " detected (",
             round(length(which(dwa$GeoRef=="detected"))/nrow(dwa),digits = 4)*100," %)",sep="\n"))
  cat(paste0(length(which(dwa$GeoRef=="multiple entries in dwa = dwa ortsname and not equal 'Bogennummer'"))," multiple entries in dwa = dwa ortsname and not equal 'Bogennummer' (",
             round(length(which(dwa$GeoRef=="multiple entries in dwa = dwa ortsname and not equal 'Bogennummer'"))/nrow(dwa),digits = 4)*100," %)",sep="\n"))
  cat(paste0(length(which(dwa$GeoRef=="multiple entries in dat = dwa ortsname"))," multiple entries in dat = dwa ortsname (",
             round(length(which(dwa$GeoRef=="multiple entries in dat = dwa ortsname"))/nrow(dwa),digits = 4)*100," %)",sep="\n"))
  cat(paste0(length(which(dwa$GeoRef=="suggested match detected"))," suggested match detected (",
             round(length(which(dwa$GeoRef=="suggested match detected"))/nrow(dwa),digits = 4)*100," %)",sep="\n"))
  cat(paste0(length(which(dwa$GeoRef=="not detected"))," not detected (",
             round(length(which(dwa$GeoRef=="not detected"))/nrow(dwa),digits = 4)*100," %)",sep="\n"))
  
  if(is.null(buffer)==F){
  # convert df to WGS84
  dwa_mol <- sp::SpatialPointsDataFrame(dwa[,12:13],dwa)
  sp::proj4string(dwa_mol) <- "+proj=moll"
  
  dwa_wgs <-sp::spTransform(dwa_mol,"+proj=longlat +datum=WGS84 +no_defs")
  # add geometry to df
  geo <- raster::geom(dwa_wgs)
  # write UTM geometry
  dwa$x <- geo[,2]
  dwa$y <- geo[,3]
  }
  return(dwa)
}# end of function
