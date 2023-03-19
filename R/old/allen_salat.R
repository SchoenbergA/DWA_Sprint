### delet unneeded tray from aleman full

ls <-c("II_55",
       "II_57",
       "II_58",
       "II_59",
       "II_60",
       "II_88",
       "III_18",
       "III_19",
       "III_20",
       "III_21",
       "III_4",
       "III_5",
       "III_24",
       "III_25",
       "III_26",
       "III_29",
       "III_30",
       "III_31",
       "III_32")

alleman_full2<-alleman_full

for (i in 1:length(ls)){
  print(ls[i])
  alleman_full2 <- alleman_full2[-which(alleman_full2$Tray_Nr==c(ls[i])),]
}

# get all empty
alm <-alm_max

head(open)
head(alleman_full2)

open$Tray_Nr <- sapply(open$open,function(x){
  spl <-stringr::str_split(x,"_")
  traynr <- paste0(spl[[1]][1],"_",spl[[1]][2])  
})

alleman_full2$`Bearbeiter/in`[which(is.na(alleman_full2$`Bearbeiter/in`))] <- "AL/JH"

write.xlsx(open,"C:/Envimaster/DWA_Sprint/R/alleman_open.xlsx")
write.xlsx(alleman_full2,"C:/Envimaster/DWA_Sprint/R/alleman_rdy.xlsx")
