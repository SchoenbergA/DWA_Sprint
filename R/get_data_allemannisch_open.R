### Get toDo sheets from alleman area

# set environment
wd <- "C:/Envimaster/DWA_Sprint/"

source(file.path(wd,"R/DWA_Sprint_functions.R"))
require(openxlsx)

### get ToDo sheets (open) from GeoRef allemanisch #############################

# load data
alm <-openxlsx::read.xlsx("C:/Envimaster/DWA_Sprint/R/Alleman_Tray_Max.xlsx")# max trays
grf <-openxlsx::read.xlsx("C:/Envimaster/DWA_Sprint/Data_Alemanisch/DWA_GeoRef_full.xlsx")# all dry places for allemanisch

# set colnames
colnames(alm)
colnames(alm)[c(1,3)]<- c("Tray_Nr","sheets_tray")

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

# filter which places are not done
open <-res[which(!res$Digi_Index%in%grf$Digi_Index),]
open <- as.data.frame(open)

# write all todo
write.xlsx(open,"C:/Envimaster/DWA_Sprint/R/Alleman_toDo.xlsx")

### bind new entries to rdy places #############################################

# load processed toDo table
todo <-openxlsx::read.xlsx("C:/Envimaster/DWA_Sprint/Data_Alemanisch/Alleman_toDo3.xlsx")

# reduce columns for equal format to DWA Sprint
colnames(grf)
grf <- grf[,c(3:7,15:18,24)]
colnames(grf) <- colnames(todo)

# bind
alleman_full <- rbind(todo,grf)
# ,add Tray_nr
alleman_full$Tray_Nr <- sapply(alleman_full$Digi_Index,function(x){
  spl <-stringr::str_split(x,"_")
  traynr <- paste0(spl[[1]][1],"_",spl[[1]][2])  
})

# check trays
check_tray_rdy_alleman(alleman_full)           
check_tray_rdy(alleman_full,max_I_III,skip_empty = T)

# check for duplicates
any(duplicated(alleman_full$Digi_Index))
alleman_full[which(duplicated(alleman_full$Digi_Index)==T),]

### get todo for selected Trays ################################################

# list all Trays which are in Pfalz data or have only minimal rdy sheets (this list was made per hand by comparion)
ls <-c("II_55","II_57","II_58","II_59","II_60","II_88","III_18","III_19",
       "III_20","III_21","III_4","III_5","III_24","III_25","III_26","III_29","III_30", "III_31", "III_32")

# get copy of all rdy in allemanisch (the georef and first todo rdy)
alleman_full2<-alleman_full

# reduce GeoRef alemanisch by all unneeded Trays
for (i in 1:length(ls)){
  print(ls[i])
  alleman_full2 <- alleman_full2[-which(alleman_full2$Tray_Nr==c(ls[i])),]
}

# get n_sheets for all Trays in allemanisch which are needed
alm_max <-check_tray_rdy(df = alleman_full2,max_bogen_df =max_I_III, T)

# loop to get all bogen nr
for(i in 1:nrow(alm_max)){
  # get all digi index
  Ind <-as.data.frame(sprintf(paste0(alm_max$Tray_Nr[i],"_%04d"),1:alm_max$sheets_tray[i]))
  colnames(Ind) <- "Digi_Index"
  if(i==1){
    res <-Ind
  } else {
    res <- rbind(res,Ind)
  }
}  

# filter which places are not done
open <-res[which(!res$Digi_Index%in%alleman_full2$Digi_Index),]
open <- as.data.frame(open)

# add Tray_Nr
open$Tray_Nr <- sapply(open$open,function(x){
  spl <-stringr::str_split(x,"_")
  traynr <- paste0(spl[[1]][1],"_",spl[[1]][2])  
})

# add Bearbeiter/in
alleman_full2$`Bearbeiter/in`[which(is.na(alleman_full2$`Bearbeiter/in`))] <- "AL/JH"

# write open and rdy
write.xlsx(open,"C:/Envimaster/DWA_Sprint/R/alleman_open.xlsx")
write.xlsx(alleman_full2,"C:/Envimaster/DWA_Sprint/R/alleman_rdy.xlsx")

# in Excel: rbind open to rdy and sort by digi index

