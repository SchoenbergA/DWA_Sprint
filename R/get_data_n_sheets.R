### DWA Sprint

# set environment
wd <- "C:/Envimaster/DWA_Sprint/"

source(file.path(wd,"R/DWA_Sprint_functions.R"))
require(openxlsx)

### get max sheet in trays 

server_path <-"Z:/arbeitsgruppen-weitere/dwa-boegen"
folder_list <-list("Bereich_I_1_bis_I_40","Bereich_II_1-II_115","Bereich_III_1_bis_III_60")
max_I_III <- get_n_bogen_in_tray(path_server = server_path,folder_list = folder_list)

# write table
write.xlsx(max_I_III,file.path(wd,"R/n_sheets_I_III.xlsx"))
