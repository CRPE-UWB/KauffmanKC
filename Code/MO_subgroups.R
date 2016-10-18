rm(list=ls())
#install.packages('readr')
library(readr)
library(dplyr)

file <- "~/Google Drive/CRPE/KauffmanKC/Data/MAP_School_Disaggregate_Final.csv" 
data <- read.csv(file, header=TRUE,stringsAsFactors = F,fileEncoding="latin1")

#datar <- read_csv(file)
#names(data)

table(data$TYPE)
table(data$CATEGORY)

data <- filter(data, CATEGORY != 'MSIP5 Special Programs')
####TEST
test1 <- data %>%
  group_by(SCHOOL_CODE,SCHOOL_NAME,TYPE,CONTENT_AREA,YEAR) %>%
  summarise(number = n(),
            REPORTED = sum(ACCOUNTABLE,na.rm=T))

filter(test1, SCHOOL_NAME == 'ADAIR CO. HIGH' & CONTENT_AREA=='Eng. Language Arts' & YEAR == 2010)

test2 <- filter(data, SCHOOL_NAME == 'ADAIR CO. HIGH' & CONTENT_AREA=='Eng. Language Arts' & YEAR == 2010)
test2$ACCOUNTABLE
####
MO_data_dsg = data %>%
  group_by(SCHOOL_CODE,SCHOOL_NAME,TYPE,CONTENT_AREA,YEAR) %>%
  summarise(number = n(),
            enrolled = sum(ACCOUNTABLE, na.rm=T), 
            participant = sum(PARTICIPANT, na.rm=T),                
            reported = sum(REPORTABLE, na.rm=T),
            level_unknown = sum(LEVEL_NOT_DETERMINED, na.rm=T),
            #level_unknown_pct = mean(LEVEL_NOT_DETERMINED_PCT, na.rm=T),   
            below_basic = sum(BELOW_BASIC, na.rm=T),
            #below_basic_pct = mean(BELOW_BASIC_PCT, na.rm=T), 
            basic = sum(BASIC, na.rm=T),                      
            #basic_pct = mean(BASIC_PCT, na.rm=T),
            proficent = sum(PROFICIENT, na.rm=T),
            #proficient_pct = mean(PROFICIENT_PCT, na.rm=T),             
            advanced = sum(ADVANCED, na.rm=T),
            #advanced_pct = mean(ADVANCED_PCT, na.rm=T),
            bottom_two = sum(BOTTOM_TWO_LEVELS, na.rm=T),          
            #bottom_two_pct = mean(BOTTOM_TWO_LEVELS_PCT, na.rm=T),
            top_two = sum(TOP_TWO_LEVELS, na.rm=T),
            #top_two_pct = mean(TOP_TWO_LEVELS_PCT, na.rm=T),         
            #throw away MAP_INDEX --> NAs
            mean_scale_score = mean(MEAN_SCALE_SCORE, na.rm=T),
            median_scale_score = mean(MEDIAN_SCALE_SCORE, na.rm=T),         
            median_terra = mean(MEDIAN_TERRANOVA, na.rm=T)
  ) 
#######

MO_data_dsg <- data.frame(MO_data_dsg)

MO_data_dsg$TYPE[MO_data_dsg$TYPE=="Amer. Indian or Alaska Native"] <-"native"
MO_data_dsg$TYPE[MO_data_dsg$TYPE=="Asian/Pacific Islander"] <-"asian"
MO_data_dsg$TYPE[MO_data_dsg$TYPE=="Black(not Hispanic)"] <-"black"
MO_data_dsg$TYPE[MO_data_dsg$TYPE=="Hispanic"] <-"latino"
MO_data_dsg$TYPE[MO_data_dsg$TYPE=="IEP_student"] <-"iep"
MO_data_dsg$TYPE[MO_data_dsg$TYPE=="LEP/ELL Students"] <-"ell"
MO_data_dsg$TYPE[MO_data_dsg$TYPE=="Map Free and Reduced Lunch"] <-"frl"
MO_data_dsg$TYPE[MO_data_dsg$TYPE=="White(not Hispanic)"] <-"white"

newdat <- filter(MO_data_dsg, CONTENT_AREA == 'Eng. Language Arts')

library(reshape2)
newdat <- newdat[,-4]
##calculate proportions by 
newdat <- newdat %>%
          mutate(below_basic_perc = below_basic/reported,
                 basic_perc = basic/reported,
                 proficent_perc = proficent/reported,
                 advanced_perc = advanced/reported)

#This data is good for graphing with the subgroups and year handles.  But if you want to convert
#to wide use the following code:
table(newdat$TYPE)

wdata <- reshape(newdat, 
             timevar = "TYPE",
             idvar = c("SCHOOL_CODE","SCHOOL_NAME","YEAR"),
             direction = "wide")

