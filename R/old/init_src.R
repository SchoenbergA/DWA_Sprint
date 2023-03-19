### DWA Sprint functions

# check and merge data #########################################################
DWA_sprint_stats <- function(path_data){
  # list all files in folder
  ls <- list.files(path_data)
  
  # load each files in ls
  ls_dat <- lapply(1:length(ls), function (x){
    dat <-openxlsx::read.xlsx(file.path(path_data,ls[x]))
    dat$tab <- ls[x]
    return(dat)
  }) # end lapply

  # check ncol for all tab
  n_col <-lapply(1:length(ls_dat), function(nc){
    n_col <-ncol(ls_dat[[nc]])-1
  })
  
  ls_df <-as.data.frame(ls)
  ls_df <-cbind(ls_df,unlist(n_col))
  n_col_issue <-ls_df[which(ls_df$`unlist(n_col)`!=10),]
  
  if(nrow(n_col_issue)>0){
    cat("Unequal number of columns detected",sep = "\n")
    print( n_col_issue)
    stop("equal number of columns in each file required")
  }
  # check for equal files
  cat("### Checking for equal files ", sep="\n")
for(id2 in 1:length(ls_dat)){
  for(id in 1:length(ls_dat)){
    if(id!=id2){
    if(identical(ls_dat[[id]][1:(ncol(ls_dat[[id]])-1)],ls_dat[[id2]][1:(ncol(ls_dat[[id2]])-1)])==T){
      res_ident <- cbind(ls[[id]],ls[[id2]])
      #cat(paste0("data ",ls[[id]]," is eqaul to ",ls[[id2]]),sep="\n")
    } else {
      res_ident <- cbind(0,0)
    }
      if(id==2&id2==1){
        res_ident2 <- as.data.frame(res_ident)
      } else {
        res_ident2 <-rbind(res_ident2,res_ident)
      }
 }
  }
}
  res_ident2 <- res_ident2[which(res_ident2$V1!="0"),]
  if(nrow(res_ident2)>0){
    cat("equal files detected ", sep="\n")
    print(res_ident2)
  } else {
    cat("No equal files detected ", sep="\n")
  }
  
  # rbind all files
  df <- do.call("rbind", ls_dat)
    
        # check df for NA
        cat(" ", sep="\n")
        cat("### Checking for NA ", sep="\n")
        if(any(is.na(df[,1:(ncol(df)-1)])==T)==T){
          cat("NA detected", sep="\n")
          cat(paste0(nrow(df[which(is.na(df$Buchstabe)| is.na(df$Digi_Index)| is.na(df$Nummer)),]), " rows with multiple NA detected"), sep="\n")
        }
        # na handling, clean all rows with multiple NA indicating any issues caused by hiwis
        df <-df[-which(is.na(df$Buchstabe) | is.na(df$Digi_Index)| is.na(df$Nummer)),]
        cat("deleting rows with NA in 'Buchstabe','Nummer','Digi_Index'", sep="\n")
        cat(" ", sep="\n")
        
  
              ### check for duplicates
        cat("### Checking for duplicated 'Digi_Index", sep="\n")
              if(any(duplicated(df$Digi_Index)==T)==T){
                cat("Duplicated 'Digi_Index' detected", sep="\n")
                duplicates <- df[which(duplicated(df$Digi_Index)==T),]
                cat(paste0(nrow(duplicates)," Duplicates detected", sep="\n"))
                cat(" ", sep="\n")
                print(unique(duplicates$tab))
                duplicates <- df[which(duplicated(df$Digi_Index,fromLast = T)==T),]
                print(unique(duplicates$tab))

                
              } else {
                cat("No duplicated 'Digi_Index' detected", sep="\n")
              }

        
        # check data
        cat(" ", sep="\n")
        cat("### Checking for Issues in 'Ortsname' ", sep="\n")
        if(any(df$Ort==999)|any(df$Ort==888)|any(df$Ort==777)|any(df$Ort=="???")|any(df$Ort=="?")){
        if(any(df$Ort==999)){
          cat(" ", sep="\n")
          cat("'Ort' containing '999' detected", sep="\n")
          print(df[which(df$Ort==999),c(1:6,11)])
        }
        if(any(df$Ort==888)){
          cat(" ", sep="\n")
          cat("'Ort' containing '888' detected", sep="\n")
          print(df[which(df$Ort==888),c(1:6,11)])
        }
        if(any(df$Ort==777)){
          cat(" ", sep="\n")
          cat("'Ort' containing '777' detected", sep="\n")
          print(df[which(df$Ort==777),c(1:6,11)])
        }
        if(any(df$Ort=="???")){
          cat(" ", sep="\n")
          cat("'Ort' containing '???' detected", sep="\n")
          print(df[which(df$Ort=="???"),c(1:6,11)])
        }
        if(any(df$Ort=="?")){
          cat(" ", sep="\n")
          cat("'Ort' containing '?' detected", sep="\n")
          print(df[which(df$Ort=="?"),c(1:6,11)])
        }
        } else {
          cat("No issues in 'Ort' detected", sep="\n")
        }
    # add Tray_Nr
    df$Tray_Nr <- sapply(df$Digi_Index,function(x){
    spl <-stringr::str_split(x,"_")
    traynr <- paste0(spl[[1]][1],"_",spl[[1]][2])  
  })
    return(df)
} # end of function

# check tray progress ##########################################################
check_tray_rdy <- function(df,max_bogen_df,skip_empty=FALSE){


  nt <-max_bogen_df
  #colnames(nt)[1] <- "pfz_data" 
  #colnames(nt) [2]<- "n_bögen"
  for(i in 1:length(unique(nt$pfz_data))){
    rdy <-length(which(df$Tray_Nr==unique(nt$pfz_data)[i])) / nt$n_bögen[which(nt$pfz_data==unique(nt$pfz_data)[i])]
    #print(round(rdy,digits = 2))
    
    # return df
    tray_nr <- unique(nt$pfz_data)[i]
    sheets_rdy <-length(which(df$Tray_Nr==unique(nt$pfz_data)[i]))
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
  row.names(res) <- c(1:nrow(res))
  colnames(res) <- c("Tray_Nr","sheets_rdy","sheets_tray","rdy_in_%")
  
  if(skip_empty==T){
  # clean
  res <- res[which(res$`rdy_in_%`>0),]
    
  }  
    


  return(res)
}

### Hiwi Stats #################################################################
HiWi_stats <- function(df){
  # get all HiWi names
hiwi <-unique(dfc$`Bearbeiter/in`)
hdf <-as.data.frame(hiwi)
  # get amount of bogen rdy for each hiwi
  for (i in 1:length(hiwi)){
    hdf$n_bogen[i] <-length(which(dfc$`Bearbeiter/in`==hiwi[i])) 
  }
# calculate effectiveness
hdf$effectiveness <- round(hdf$n_bogen/nrow(dfc)*100,digits = 2)
print(hdf)
print(sum(hdf$effectiveness))
}
################################################################################
################# Test area ####################################################

# setup environment

# load packages
require(openxlsx)
require(stringr)

# load paths
path_pfz_data <- "C:/Envimaster/DWA_Sprint/Data_Pfalz" # all pfz data
path_data <- "C:/Envimaster/DWA_Sprint/Data" # all sprint data without pfz
path_ale_data <- "C:/Envimaster/DWA_Sprint/Data_Alemanisch" # all alleman
aleman <- read.xlsx( "C:/Envimaster/DWA_Sprint/Data_Alemanisch/DWA_GeoRef_full.xlsx")
max_n_bogen <- read.xlsx("C:/Envimaster/DWA_Sprint/R/Alleman_Tray_Max.xlsx")
max_n_bogen2 <- read.xlsx("C:/Envimaster/DWA_Sprint/R/Pfalz_Tray_max.xlsx")

# test with get_max function (used test as varname)
colnames(max_I_III)
colnames(max_I_III)[1:2] <-c("pfz_data","n_bögen")

colnames(aleman)
df <- aleman
df$Tray_Nr <- sapply(df$Digi_Index,function(x){
  spl <-stringr::str_split(x,"_")
  traynr <- paste0(spl[[1]][1],"_",spl[[1]][2])  
})

aleman <-df

# run fun
dfc <- DWA_sprint_stats(path_pfz_data)
dfc <- DWA_sprint_stats(path_data = path_ale_data)
check_tray_rdy(df = dfc,max_bogen_df =max_I_III, T)

HiWi_stats(dfc)





alleman_full
dfc[which(dfc$Tray_Nr=="III_58"),]
dfc[which(dfc$Digi_Index=="II_58_0067"),]

#write.xlsx(dfc,"C:/Envimaster/DWA_Sprint/GeoRef/Data_GeoRef/pfalz_places.xlsx")

any(dfc$Digi_Index%in%test$Digi_Index)
dfc[which(dfc$Digi_Index%in%test$Digi_Index),]
