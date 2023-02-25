### get n_bogen for DWA folders
# Description: Get the amount of Bogen in selected Trays to check if DWA Place_Ref is complete

# setup environment
require(stringr)

path <- "Z:/arbeitsgruppen-weitere/dwa-boegen" # pat to Server

list.files(path)
get_n_bogen_in_tray <- function(path_server,folder_list){
  
for(f in 1:length(folder_list)){
  folder_name <-folder_list[[f]]
# trays in folder
ty <- list.files(file.path(path,folder_name))

for(i in 1:length(ty)){
  cat(paste0("calculating n_sheets for tray ",ty[[i]]),sep = "\n")
  ls <-list.files(file.path(path,folder_name,ty[[i]]),pattern = "*\\.TIF")
  df <-as.data.frame(ls)
  
  df$index <- sapply(df$ls,function(x){
    spl <-stringr::str_split(x,"_")
    spl2 <-stringr::str_split(spl[[1]][3],c("-"))
    
    tray <-as.numeric(spl2[[1]][1]) 
  })
  # number of bogen indepenent if there are missing index numbers or diffenret amount of pages
  length(unique(df$index))
}

# apply
ls_ty <- lapply(1:length(ty), function(i){
  ls <-list.files(file.path(path,folder_name,ty[[i]]),pattern = "*\\.TIF")
  df <-as.data.frame(ls)
  
  df$index <- sapply(df$ls,function(x){
    spl <-stringr::str_split(x,"_")
    spl2 <-stringr::str_split(spl[[1]][3],c("-"))
    
    tray <-as.numeric(spl2[[1]][1]) 
  })
  
  # number of bogen indepenent if there are missing index numbers or diffenret amount of pages
  return(length(unique(df$index)))
})
cat("ready",sep = "\n")
res <-as.data.frame(ty)
res <- cbind(res,unlist(ls_ty))
colnames(res)<-c("Tray_Nr","n_sheets")

# add Roman and Number columns for sorting
res$RomanSector <-sapply(res$Tray_Nr,function(x){
  spl <-stringr::str_split(x,"_")
  tray <-spl[[1]][1] 
})
res$Tray_numeric <-sapply(res$Tray_Nr,function(x){
  spl <-stringr::str_split(x,"_")
  tray <-as.numeric(spl[[1]][2] )
})


if(f==1){
  res_df <- res
} else {
  res_df <- rbind(res_df,res)
}

}
return(res_df)

} # end of function

# run
server_path <-"Z:/arbeitsgruppen-weitere/dwa-boegen"
folder_list <-list("Bereich_I_1_bis_I_40","Bereich_II_1-II_115","Bereich_III_1_bis_III_60")

test <- get_n_bogen_in_tray(path_server = path,folder_list = folder_list)

