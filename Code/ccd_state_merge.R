#####Merge with ccd file (download from aws and locate in your directory) see "CCD_prep.R" for specifics
rm(list=ls())
library(dplyr)

MO_ccd <- readRDS("~/Google Drive/CRPE/KauffmanKC/Data/ccd/ccd_2010_2014_city.Rda")
MO_state <- readRDS("~/Google Drive/CRPE/KauffmanKC/Data/MO_complete_enprf.Rda")
str(MO_ccd)
str(MO_state)
#Change factor id to string 
MO_ccd$seasch <- as.character(MO_ccd$seasch)

#merge ccd to state performance file by id and year 
#We only need school information not enrollments
names(MO_ccd)

MO_ccd <- MO_ccd[,c(1:16,33:35)]

MO_ccd <- MO_ccd[!duplicated(MO_ccd[,c("seasch")]),]
#119 non matchches to state file (majority are missing a school name)
MO_complete <- left_join(MO_state,MO_ccd, by=c("seasch")) #keep all records in the state file
####
saveRDS(MO_complete, "~/Google Drive/CRPE/KauffmanKC/Data/MO_complete.rda")
write.csv(MO_complete, "~/Google Drive/CRPE/KauffmanKC/Data/MO_complete.csv")
