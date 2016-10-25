rm(list=ls())
#install.packages('readr')
library(readr)
library(dplyr)

file <- "~/Google Drive/CRPE/KauffmanKC/Data/MO_enrollment.csv" 
data <- read.csv(file, header=TRUE,stringsAsFactors = F,fileEncoding="latin1")

#######
str(data)
table(data$YEAR)

data <- filter(data, YEAR == 2010 | YEAR == 2011 | YEAR == 2012 | YEAR == 2013 | YEAR == 2014 |YEAR == 2015 | YEAR == 2016 )


##[OPTIONAL] create CCD type ID 
names(data)
data$district_code_string <- sprintf("%06s", as.character(data$COUNTY_DISTRICT_CODE)) 
#Combine to match ccd id
data$sch_code_string <- as.character(data$SCHOOL_CODE)
library(tidyr)

data <- unite(data, seasch, sch_code_string,district_code_string, sep='')
####
#Attach performance data 
PRFdata <- readRDS("~/Google Drive/CRPE/KauffmanKC/Data/MO2010_2016prf.Rda")

names(PRFdata)
#get columns that we want
PRFdata <- PRFdata[, c(3:5,17:22)]
PRFdata <- filter(PRFdata, content_area == "Mathematics" | content_area == "Eng. Language Arts")
###make wide 
wdata <- reshape(PRFdata, 
             timevar = "content_area",
             idvar = c("sch_name","year","seasch"),
             direction = "wide")

saveRDS(wdata, file="~/Google Drive/CRPE/KauffmanKC/Data/MO2010_2016prf_wide.Rda")
##########################
#see how many don't match
nomatch <- anti_join(data,wdata, by=c("seasch","YEAR"="year"))
#119 non matchches to state file (majority are missing a school name)
MO_complete <- full_join(data,wdata, by=c("seasch","YEAR"="year"))#keep all records in the state file
####
saveRDS(MO_complete, "~/Google Drive/CRPE/KauffmanKC/Data/MO_complete_enprf.rda")
write.csv(MO_complete, "~/Google Drive/CRPE/KauffmanKC/Data/MO_complete_enprf.csv")

