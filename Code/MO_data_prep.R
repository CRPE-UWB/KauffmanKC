
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
            group_by(SCHOOL_CODE_0001,SCHOOL_NAME,CONTENT_AREA,YEAR) %>%
              summarise(number = n(),
              enrolled = sum(ACCOUNTABLE, na.rm=T), 
              participant = sum(PARTICIPANT, na.rm=T),                
              reported = sum(REPORTABLE, na.rm=T),
              level_unknown = sum(LEVEL_NOT_DETERMINED, na.rm=T),
              level_unknown_pct = mean(LEVEL_NOT_DETERMINED_PCT, na.rm=T),   
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

#We don't want to use the aggregated percentages, we can recalculate with the counts (more reliable in this case)
#Recalculate percent

MO_data <- MO_data %>%
  mutate(below_basic_perc = below_basic/reported,
         basic_perc = basic/reported,
         proficent_perc = proficent/reported,
         advanced_perc = advanced/reported)

#####Merge with ccd file (download from aws and locate in your directory) see "CCD_prep" for specifics

