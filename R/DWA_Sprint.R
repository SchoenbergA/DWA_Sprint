### DWA Sprint

# setup environment

# load packages
require(openxlsx)
require(stringr)

source("C:/Envimaster/DWA_Sprint/R/DWA_Sprint_functions.R")

# load paths
path_pfz_data <- "C:/Envimaster/DWA_Sprint/Data_Pfalz" # all pfz data
path_data <- "C:/Envimaster/DWA_Sprint/Data" # all sprint data without pfz

# load n_sheet table
n_sheets_I_III <- read.xlsx("C:/Envimaster/DWA_Sprint/R/n_sheets_I_III.xlsx")

alm1 <- read.xlsx( "C:/Envimaster/DWA_Sprint/Data_Alemanisch/alleman_toDo_full_area.xlsx")
alm2 <- read.xlsx( "C:/Envimaster/DWA_Sprint/Data_Alemanisch/alleman_toDo_full_area2.xlsx")
head(alm1)
head(alm2)

df_alm <- rbind(alm1,alm2)

length(which(df_alm$Tray_Nr=="II_92"))

# add "tab" col and reorder
alm$tab <- "AL/JH/VD_special"
alm<- alm[,c(1:10,12,11)]

# check and merge data
df_pfz <- DWA_sprint_stats(path_pfz_data)
df_spr <- DWA_sprint_stats(path_data = path_data)
df_alm <- alm # function not needed due to full df instead of multiple tables



# check tray progress
check_tray_rdy(df = df_pfz,max_bogen_df =n_sheets_I_III,skip_empty =  T)
check_tray_rdy(df = df_spr,max_bogen_df =n_sheets_I_III, T)
check_tray_rdy(df = df_alm,max_bogen_df =n_sheets_I_III, T)

# write
tray_spr <-check_tray_rdy(df = df_spr,max_bogen_df =n_sheets_I_III, T)
write.xlsx(tray_spr,"C:/Envimaster/DWA_Sprint/sprint_result.xlsx")

tray_pfz <-check_tray_rdy(df = df_pfz,max_bogen_df =n_sheets_I_III, T)
write.xlsx(tray_pfz,"C:/Envimaster/DWA_Sprint/sprint_result_pfz.xlsx")



# check hiwi status
HiWi_stats(df_pfz)
HiWi_stats(df_I)

### rbind all table

# add id for org data
df_pfz$org_tab <-"df_pfz"
df_spr$org_tab <-"df_spr"
df_alm$org_tab <-"df_alm"

df_full <- rbind(df_pfz,df_spr,df_alm)
check_df <-check_tray_rdy(df = df_full,max_bogen_df =n_sheets_I_III, T)
check_df

# trays more than 100% rdy
check_df[which(check_df$`rdy_in_%`>100),]
# trays not rdy
check_df[which(check_df$`rdy_in_%`<100),]


# check issues by Tray_Nr

check_issue <- function(tray_code){
      cat(paste0("HiWi:       ",unique(df_full$`Bearbeiter/in`[which(df_full$Tray_Nr==tray_code)],collapse = ", ")),sep="\n")
      cat(paste0("Table:      ",unique(df_full$`tab`[which(df_full$Tray_Nr==tray_code)],collapse = ", ")),sep="\n")
      cat(paste0("Data.frame: ",unique(df_full$org_tab[which(df_full$Tray_Nr==tray_code)],collapse = ", ")),sep="\n")
}

check_issue("III_3")
check_issue("III_5")
check_issue("III_6")

HiWi_stats(df_full)
