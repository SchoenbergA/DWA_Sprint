require(openxlsx)

# load data
alm <-openxlsx::read.xlsx("C:/Envimaster/DWA_Sprint/R/Alleman_Tray_Max.xlsx")
grf <-openxlsx::read.xlsx("C:/Envimaster/DWA_Sprint/Data_Alemanisch/DWA_GeoRef_full.xlsx")


  alm <- alm_max
# loop to get all bogen nr
for(i in 1:nrow(alm)){
  # get all digi index
  Ind <-as.data.frame(sprintf(paste0(alm$Tray_Nr[i],"_%04d"),1:alm$sheets_tray[i]))
  colnames(Ind) <- "Digi_Index"
  if(i==1){
    res <-Ind
  } else {
    res <- rbind(res,Ind)
  }
}  
class(res)
class(open)
# which Digi_Index all allredy transliterated?
open <-res[which(!res$Digi_Index%in%grf$Digi_Index),]
open <-res[which(!res$Digi_Index%in%alleman_full2$Digi_Index),]
open <- as.data.frame(open)
write.xlsx(open,"C:/Envimaster/DWA_Sprint/R/Alleman_toDo.xlsx")
  # the rdy table -> put digi_index to col like template and add header from template in Ecxel

check_tray_rdy_alleman <- function(df){
  # add tray nr
  df$Tray_Nr <- sapply(df$Digi_Index,function(x){
    spl <-stringr::str_split(x,"_")
    traynr <- paste0(spl[[1]][1],"_",spl[[1]][2])  
  })
  # load n_bogen in tray data
  nt <-openxlsx::read.xlsx("C:/Envimaster/DWA_Sprint/R/Alleman_Tray_Max.xlsx")
  
  for(i in 1:length(unique(nt$pfz_data))){
    #rdy <-length(which(df$Tray_Nr==unique(nt$pfz_data)[i])) / nt$n_bögen[which(nt$pfz_data==unique(nt$pfz_data)[i])]
    rdy <-length(which(is.na(df$Ort[which(df$Tray_Nr==unique(nt$pfz_data)[i])])==F)) / nt$n_bögen[which(nt$pfz_data==unique(nt$pfz_data)[i])]
    print(round(rdy,digits = 2))
    
    # return df
    tray_nr <- unique(nt$pfz_data)[i]
    sheets_rdy <-length(which(is.na(df$Ort[which(df$Tray_Nr==unique(nt$pfz_data)[i])])==F))
    sheets_tray <-nt$n_bögen[which(nt$pfz_data==unique(nt$pfz_data)[i])]
    rdy_percent <-round(rdy,digits = 2)*100
    
    
    new_row <- c(tray_nr,sheets_rdy,sheets_tray,rdy_percent)
    
    
    if(i==1){
      res <-new_row
    } else {
      res <- rbind(res,new_row)
    }
  }
  res <- as.data.frame(res)
  row.names(res) <- c(1:14)
  colnames(res) <- c("Tray_Nr","sheets_rdy","sheets_tray","rdy_in_%")
  return(res)
}

# load toDo table
todo <-openxlsx::read.xlsx("C:/Envimaster/DWA_Sprint/Data_Alemanisch/Alleman_toDo3.xlsx")

# get grf oin format for DWA Sprint
colnames(grf)
grf <- grf[,c(3:7,15:18,24)]
colnames(grf) <- colnames(todo)

# bind
alleman_full <- rbind(todo,grf)
alleman_full$Tray_Nr <- sapply(alleman_full$Digi_Index,function(x){
  spl <-stringr::str_split(x,"_")
  traynr <- paste0(spl[[1]][1],"_",spl[[1]][2])  
})

# check trays
check_tray_rdy_alleman(alleman_full)           

any(duplicated(alleman_full$Digi_Index))
alleman_full[which(duplicated(alleman_full$Digi_Index)==T),]

### get rdy alemanisch
colnames(grf)
colnames(todo)

test <- rbind(grf,todo)
any(duplicated(test$Digi_Index))
test[which(duplicated(test$Digi_Index)),]
check_tray_rdy_alleman(test)