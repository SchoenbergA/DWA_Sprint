### DWA Sprint functions

### get n_bogen for DWA folders ################################################
# Description: Get the amount of Bogen in selected Trays to check if DWA Place_Ref is complete

get_n_bogen_in_tray <- function(path_server,folder_list){
  
  for(f in 1:length(folder_list)){
    folder_name <-folder_list[[f]]
    # trays in folder
    ty <- list.files(file.path(path_server,folder_name))
    
    for(i in 1:length(ty)){
      cat(paste0("calculating n_sheets for tray ",ty[[i]]),sep = "\n")
      ls <-list.files(file.path(path_server,folder_name,ty[[i]]),pattern = "*\\.TIF")
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
      ls <-list.files(file.path(path_server,folder_name,ty[[i]]),pattern = "*\\.TIF")
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

## run example
#server_path <-"Z:/arbeitsgruppen-weitere/dwa-boegen"
#folder_list <-list("Bereich_I_1_bis_I_40","Bereich_II_1-II_115","Bereich_III_1_bis_III_60")
#max_I_III <- get_n_bogen_in_tray(path_server = server_path,folder_list = folder_list)

# check and merge data #########################################################
# Description: Merge all files in folder and check data. Output:merged dataframe
DWA_sprint_stats <- function(path_data){
  # list all files in folder (only xlsx to ignore subfolders)
  ls <- list.files(path_data,pattern = "*\\.xlsx")
  
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
  
  # trim both leading amnd tailing whitespaces
  cat("Trimming Whitspaces ", sep="\n")
  for (i in 1:ncol(df)) {
    df[,i] <-str_trim(df[,i], "both") 
  }
  
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
  
  # check HiWi names
  cat("### Checking for NA in column 'Bearbeiter/in' ", sep="\n")
  hiwi_na <- unique(df$tab[which(is.na(df$`Bearbeiter/in`))])
  
  if(length(hiwi_na>0)){
    cat("NA in column 'Bearbeiter/in' detected", sep="\n")
    print(hiwi_na)
    cat(" ", sep="\n")
  } else {
    cat("No issues in 'Bearbeiter/in' detected", sep="\n")
    cat(" ", sep="\n")
  }
  
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
  
  # check for is na in Ort
  if(any(is.na(df$Ort))){
    cat(" ", sep="\n")
    cat("NA in 'Ort' detected. Setting NA to '999", sep="\n")
  df$Ort[is.na(df$Ort)] <- 999
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

# run example
#path_pfz_data <- "C:/Envimaster/DWA_Sprint/Data_Pfalz" # all pfz data
#dfc <- DWA_sprint_stats(path_data = path_pfz_data)

# check tray progress ##########################################################
check_tray_rdy <- function(df,max_bogen_df,skip_empty=FALSE){
  

  nt <-max_bogen_df
  for(i in 1:length(unique(nt$Tray_Nr))){
    rdy <-length(which(df$Tray_Nr==unique(nt$Tray_Nr)[i])) / nt$n_sheets[which(nt$Tray_Nr==unique(nt$Tray_Nr)[i])]
    #print(round(rdy,digits = 2))
    
    # return df
    tray_nr <- unique(nt$Tray_Nr)[i]
    sheets_rdy <-length(which(df$Tray_Nr==unique(nt$Tray_Nr)[i]))
    sheets_tray <-nt$n_sheets[which(nt$Tray_Nr==unique(nt$Tray_Nr)[i])]
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

  # convert to numeric
  res$sheets_rdy <- as.numeric(res$sheets_rdy)
  res$sheets_tray <- as.numeric(res$sheets_tray)
  res$`rdy_in_%` <- as.numeric(res$`rdy_in_%`)
  
  if(skip_empty==T){
    # clean
    res <- res[which(res$`rdy_in_%`>0),]
    
  }  
  
  
  
  return(res)
}

# run example
#check_tray_rdy(df = dfc,max_bogen_df = max_I_III,skip_empty = T)

### Hiwi Stats #################################################################
HiWi_stats <- function(df){
  # get all HiWi names
  hiwi <-unique(df$`Bearbeiter/in`)
  hdf <-as.data.frame(hiwi)
  # get amount of bogen rdy for each hiwi
  for (i in 1:length(hiwi)){
    hdf$n_bogen[i] <-length(which(df$`Bearbeiter/in`==hiwi[i])) 
  }
  # calculate effectiveness
  hdf$effectiveness <- round(hdf$n_bogen/nrow(df)*100,digits = 2)
  print(hdf)
  print(sum(hdf$effectiveness))
}

# run example
#HiWi_stats(dfc)
