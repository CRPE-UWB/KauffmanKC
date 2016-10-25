rm(list=ls())
#install.packages('readr')
library(readr)
library(dplyr)

file <- "~/Google Drive/CRPE/KauffmanKC/Data/MAP_School_Disaggregate_Final.csv" 
data <- read.csv(file, header=TRUE,stringsAsFactors = F,fileEncoding="latin1")

#######

table(data$TYPE)

#data <- filter(data, TYPE != "MAP Free and Reduced Lunch")
class(data)

data$TYPE[data$TYPE=="Amer. Indian or Alaska Native"] <-"native"
data$TYPE[data$TYPE=="Asian/Pacific Islander"] <-"asian"
data$TYPE[data$TYPE=="Black(not Hispanic)"] <-"black"
data$TYPE[data$TYPE=="Hispanic"] <-"latino"
data$TYPE[data$TYPE=="IEP_student"] <-"iep"
data$TYPE[data$TYPE=="LEP/ELL Students"] <-"ell"
data$TYPE[data$TYPE=="Map Free and Reduced Lunch"] <-"frl"
data$TYPE[data$TYPE=="MAP Free and Reduced Lunch"] <-"frl2.0"
data$TYPE[data$TYPE=="White(not Hispanic)"] <-"white"
#########Calculate Enrollment numbers by Race 
MO_data_dsg = data %>%
  group_by(COUNTY_DISTRICT,SCHOOL_CODE,SCHOOL_NAME,TYPE,YEAR) %>%
  summarise(
            enrolled = sum(ACCOUNTABLE, na.rm=T)) 
            #participant = sum(PARTICIPANT, na.rm=T),                
            #reported = sum(REPORTABLE, na.rm=T))
            
MO_data_dsg <- data.frame(MO_data_dsg)

wdata <- reshape(MO_data_dsg, 
             timevar = "TYPE",
             idvar = c("COUNTY_DISTRICT","SCHOOL_CODE","SCHOOL_NAME","YEAR"),
             direction = "wide")


#####################
library(readr)

file15 <- "~/Google Drive/CRPE/KauffmanKC/Data/MO_2015.csv" 
data15 <- read_delim(file15, delim = ',')

data15 <- filter(data15, TYPE == "Total")

data15 <- data15[, c("COUNTY_DISTRICT","SCHOOL_CODE","SCHOOL_NAME","CONTENT_AREA","YEAR",
                     "ACCOUNTABLE","REPORTABLE","LEVEL_NOT_DETERMINED",
                     "BELOW_BASIC","BASIC","PROFICIENT","ADVANCED",
                     "BELOW_BASIC_PCT","BASIC_PCT","PROFICIENT_PCT",
                     "ADVANCED_PCT","TOP_TWO_LEVELS_PCT")]
#clean up "*" 
data15[ ,]  <- lapply(data15[ ,] , 
                      FUN = function(x) {x[x == '*'] <- NA; x})
#create empty columns to append to other years were this is not missing
data15$top_two <- NA
data15$mean_scale_score <- NA
data15$median_scale_score <- NA
data15$median_terra <- NA 
####Rename and reorder to append 
#reorder
data15<- data15[,c("COUNTY_DISTRICT","SCHOOL_CODE","SCHOOL_NAME","CONTENT_AREA","YEAR",                
                   "ACCOUNTABLE","REPORTABLE","LEVEL_NOT_DETERMINED","BELOW_BASIC",         
                   "BASIC","PROFICIENT","ADVANCED", "top_two",
                   "mean_scale_score","median_scale_score","median_terra",
                   "BELOW_BASIC_PCT","BASIC_PCT","PROFICIENT_PCT","ADVANCED_PCT","TOP_TWO_LEVELS_PCT")]
#rename
names(data15) <- c("district_code","sch_code","sch_name","content_area","year",              
                   "enrolled","reported","level_unknown","below_basic",       
                   "basic","proficent","advanced","top_two",           
                   "mean_scale_score","median_scale_score","median_terra","below_basic_perc",  
                   "basic_perc","proficent_perc","advanced_perc","advPrf")

#append

APPdata <- rbind(MO_data,data15)