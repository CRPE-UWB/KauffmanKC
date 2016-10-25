
rm(list=ls())
library(dplyr)
file <- "~/Google Drive/CRPE/KauffmanKC/Data/MAP_School_Final.csv" 
data <- read.csv(file, header=TRUE,stringsAsFactors = F,fileEncoding="latin1")

names(data)

#look at the cases per year 2010-2014
table(data$YEAR)
#look at the subset categories FRL, subgroup, etc.
table(data$CATEGORY)
#We only want to look at school totals 

data <- filter(data, CATEGORY == "Total")
###

test1 <- data %>%
  group_by(SCHOOL_CODE_0001,SCHOOL_NAME,CONTENT_AREA,YEAR) %>%
  summarise(number = n(),
            REPORTED = sum(REPORTABLE,na.rm=T))

filter(test1, SCHOOL_NAME == 'ADAIR CO. HIGH' & CONTENT_AREA=='Eng. Language Arts' & YEAR == 2010)

test2 <- filter(data, SCHOOL_NAME == 'ADAIR CO. HIGH' & CONTENT_AREA=='Eng. Language Arts' & YEAR == 2010)
test2$REPORTABLE

#We're good.
#We need to aggregate across all relevant columns 
MO_data = data %>%
            group_by(COUNTY_DISTRICT,SCHOOL_CODE_0001,SCHOOL_NAME,CONTENT_AREA,YEAR) %>%
              summarise(#number = n(),
              enrolled = sum(ACCOUNTABLE, na.rm=T), 
              #participant = sum(PARTICIPANT, na.rm=T),                
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
              #bottom_two = sum(BOTTOM_TWO_LEVELS, na.rm=T),          
              #bottom_two_pct = mean(BOTTOM_TWO_LEVELS_PCT, na.rm=T),
              top_two = sum(TOP_TWO_LEVELS, na.rm=T), #ADVANCED + PROFICIENT
              #top_two_pct = mean(TOP_TWO_LEVELS_PCT, na.rm=T),         
              #throw away MAP_INDEX --> NAs
              mean_scale_score = mean(MEAN_SCALE_SCORE, na.rm=T),
              median_scale_score = mean(MEDIAN_SCALE_SCORE, na.rm=T),         
              median_terra = mean(MEDIAN_TERRANOVA, na.rm=T)
              )

#We don't want to use the aggregated percentages, we can recalculate with the counts (more reliable in this case)
#Recalculate percent

MO_data <- MO_data %>%
  mutate(below_basic_perc = (below_basic/reported) * 100,
         basic_perc = (basic/reported) * 100,
         proficent_perc = (proficent/reported) * 100,
         advanced_perc = (advanced/reported) * 100,
         advPrf = (top_two/reported) * 100)

MO_data <- data.frame(MO_data)
#Clean up names (optional unless we are appending)
names(MO_data) <- c("district_code","sch_code","sch_name","content_area","year",              
                    "enrolled","reported","level_unknown","below_basic",       
                    "basic","proficent","advanced","top_two",           
                    "mean_scale_score","median_scale_score","median_terra","below_basic_perc",  
                    "basic_perc","proficent_perc","advanced_perc","advPrf")   
##2015 file is seperate 
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
## Additional considerations
#in the CCD the seasch variable corresponds to the sch_id and district_code combined
#however in the ccd it has leading zeros for the district and a total of 6 digits
#this will not match our MO file and you have to add the leading "0"'s to the district code 
#and combine with the school id...

#Use sprintf function to add leading "0"'s for a total of 6 digits:
#example district_id = 1090 new variable is 001090 
#district_id = 1403921 new variable is 1403921
MO_data$district_code_string <- sprintf("%06s", as.character(MO_data$district_code)) 
#Combine to match ccd id
MO_data$sch_code_string <- as.character(MO_data$sch_code)
library(tidyr)
MO_data <- MO_data %>%
  unite(seasch, sch_code_string,district_code_string, sep='')
####
data15$district_code_string <- sprintf("%06s", as.character(data15$district_code)) 
#Combine to match ccd id
data15$sch_code_string <- as.character(data15$sch_code)
library(tidyr)
data15 <- data15 %>%
  unite(seasch, sch_code_string,district_code_string, sep='')
###
#append
APPdata <- rbind(MO_data,data15)
#Save 
saveRDS(APPdata, file="~/Google Drive/CRPE/KauffmanKC/Data/MO2010_2016prf.Rda")
write.csv(APPdata, file="~/Google Drive/CRPE/KauffmanKC/Data/MO2010_2016prf.csv")
#the 2010-2014 data 
saveRDS(MO_data, file="~/Google Drive/CRPE/KauffmanKC/Data/MO2010_2014prf.Rda")
###Save 2015-2016 file seperetly---CCD is not available for these years
saveRDS(data15, file="~/Google Drive/CRPE/KauffmanKC/Data/MO2015_2016prf.Rda")
write.csv(data15, file="~/Google Drive/CRPE/KauffmanKC/Data/MO2015_2016prf.csv")
############

