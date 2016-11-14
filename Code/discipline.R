#Discipline MO 
#11/11/2016
rm(list=ls())
#install.packages('readr')
library(readr)
library(dplyr)

file <- "~/Google Drive/CRPE/KauffmanKC/Data/MO_discipline.csv" 
MO_dis <- read.csv(file, header=TRUE,stringsAsFactors = F,fileEncoding="latin1")
#Clean and prepare
names(MO_dis)
MO_dis$district_code_string <- sprintf("%06s", as.character(MO_dis$COUNTY_DISTRICT_CODE)) 
#Combine to match ccd id
MO_dis$sch_code_string <- as.character(MO_dis$SCHOOL_CODE)
library(tidyr)
MO_dis <- MO_dis %>%
  unite(seasch, sch_code_string,district_code_string, sep='')

MO_dis <- MO_dis[which(MO_dis$YEAR>=2010 & MO_dis$YEAR<=2016), ]
table(MO_dis$YEAR)
#attach appropriate information CCD and city district parameters
data <- readRDS("~/Google Drive/CRPE/KauffmanKC/Data/KC2010_2014_city_district.rda")


#change all factors to characters
for(i in 1:dim(data)[2]) {
  if(class(data[,i]) == "factor")
    data[,i] <- as.character(data[,i])
}
#change only certain columns to to numeric 
for(i in 34:43) {
  if(class(data[,i]) == "character")
    data[,i] <- as.numeric(data[,i])
}

names(data)

dinfo <- data[,c(1,32,50:62)]
######
table(data$YEAR)
#############
table(MO_dis$YEAR)
###
nomatch <- anti_join(dinfo,MO_dis, by=c("seasch","YEAR"="YEAR"))
#119 non matchches to state file (majority are missing a school name)
MO_discipline <- inner_join(dinfo,MO_dis, by=c("seasch","YEAR"="YEAR"))#keep all records in the state file
####dostrict 

districtDis <- MO_discipline %>%
            filter(district == 'Kansas City 33 School District') %>%
            group_by(YEAR) %>%
            summarise(DRate = (sum(DISCIPLINE_INCIDENTS,na.rm=T)/sum(ENROLLMENT_GRADES_K_12,na.rm=T)) *100,
                      SRate = (sum(DISCIPLINE_REMOVAL_OUT_SCHL_SUSP,na.rm=T)/sum(ENROLLMENT_GRADES_K_12,na.rm=T)) *100)
#city
cityDis <- MO_discipline %>%
  filter(city == 'Kansas City city') %>%
  group_by(YEAR) %>%
  summarise(DRate = (sum(DISCIPLINE_INCIDENTS,na.rm=T)/sum(ENROLLMENT_GRADES_K_12,na.rm=T)) *100,
            SRate = (sum(DISCIPLINE_REMOVAL_OUT_SCHL_SUSP,na.rm=T)/sum(ENROLLMENT_GRADES_K_12,na.rm=T)) *100)

