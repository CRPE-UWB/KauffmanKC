#CCD File preparation
#2010-2014
#Variable extraction for all years
#Jose M Hernandez
#10/17/2016

rm(list=ls())
#install.packages('readr')
library(readr)
library(dplyr)

file09 <- "~/Google Drive/CRPE/ccd/sc092a.txt" 
data09 <- read_delim(file09, delim = '\t')
#only take what is needed 
data09 <- data09 %>%
  select(NCESSCH, FIPST,LEAID,SCHNO,STID09,SEASCH09,LEANM09,SCHNAM09,
          TYPE09,STATUS09,ULOCAL09,LATCOD09,LONCOD09,LEVEL09,MAGNET09,CHARTR09,TOTFRL09,
          MEMBER09,AM09,ASIAN09,HISP09,BLACK09,WHITE09,PACIFIC09,TR09,TOTETH09) %>%
  rename(ncessch=NCESSCH, fipst=FIPST,leaid=LEAID,schno=SCHNO,stid=STID09,seasch=SEASCH09,leanm=LEANM09,schnam=SCHNAM09,
         type=TYPE09,status=STATUS09,ulocal=ULOCAL09,latcod=LATCOD09,loncod=LONCOD09,level=LEVEL09,magnet=MAGNET09,chartr=CHARTR09,totfrl=TOTFRL09,
         member=MEMBER09,am=AM09,asian=ASIAN09,latino=HISP09,black=BLACK09,white=WHITE09,pacific=PACIFIC09,tr=TR09,toteth=TOTETH09)

data09$year <- 2010
##2011
file10 <- "~/Google Drive/CRPE/ccd/sc102a.txt" 
data10 <- read_delim(file10, delim = '\t')

#only take what is needed 
data10 <- data10 %>%
  select(NCESSCH, FIPST,LEAID,SCHNO,STID,SEASCH,LEANM,SCHNAM,
         TYPE,STATUS,ULOCAL,LATCOD,LONCOD,LEVEL,MAGNET,CHARTR,TOTFRL,
         MEMBER,AM,ASIAN,HISP,BLACK,WHITE,PACIFIC,TR,TOTETH) %>%
  rename(ncessch=NCESSCH, fipst=FIPST,leaid=LEAID,schno=SCHNO,stid=STID,seasch=SEASCH,leanm=LEANM,schnam=SCHNAM,
         type=TYPE,status=STATUS,ulocal=ULOCAL,latcod=LATCOD,loncod=LONCOD,level=LEVEL,magnet=MAGNET,chartr=CHARTR,totfrl=TOTFRL,
         member=MEMBER,am=AM,asian=ASIAN,latino=HISP,black=BLACK,white=WHITE,pacific=PACIFIC,tr=TR,toteth=TOTETH)
data10$year <- 2011
##2012
file11 <- "~/Google Drive/CRPE/ccd/sc111a_supp.txt" 
data11 <- read_delim(file11, delim = '\t')

#only take what is needed 
data11 <- data11 %>%
  select(NCESSCH, FIPST,LEAID,SCHNO,STID,SEASCH,LEANM,SCHNAM,
         TYPE,STATUS,ULOCAL,LATCOD,LONCOD,LEVEL,MAGNET,CHARTR,TOTFRL,
         MEMBER,AM,ASIAN,HISP,BLACK,WHITE,PACIFIC,TR,TOTETH) %>%
  rename(ncessch=NCESSCH, fipst=FIPST,leaid=LEAID,schno=SCHNO,stid=STID,seasch=SEASCH,leanm=LEANM,schnam=SCHNAM,
         type=TYPE,status=STATUS,ulocal=ULOCAL,latcod=LATCOD,loncod=LONCOD,level=LEVEL,magnet=MAGNET,chartr=CHARTR,totfrl=TOTFRL,
         member=MEMBER,am=AM,asian=ASIAN,latino=HISP,black=BLACK,white=WHITE,pacific=PACIFIC,tr=TR,toteth=TOTETH)
data11$year <- 2012
#2013
#only take what is needed 
file12 <- "~/Google Drive/CRPE/ccd/sc122a.txt" 
data12 <- read_delim(file12, delim = '\t')

data12 <- data12 %>%
  select(NCESSCH, FIPST,LEAID,SCHNO,STID,SEASCH,LEANM,SCHNAM,
         TYPE,STATUS,ULOCAL,LATCOD,LONCOD,LEVEL,MAGNET,CHARTR,TOTFRL,
         MEMBER,AM,ASIAN,HISP,BLACK,WHITE,PACIFIC,TR,TOTETH) %>%
  rename(ncessch=NCESSCH, fipst=FIPST,leaid=LEAID,schno=SCHNO,stid=STID,seasch=SEASCH,leanm=LEANM,schnam=SCHNAM,
         type=TYPE,status=STATUS,ulocal=ULOCAL,latcod=LATCOD,loncod=LONCOD,level=LEVEL,magnet=MAGNET,chartr=CHARTR,totfrl=TOTFRL,
         member=MEMBER,am=AM,asian=ASIAN,latino=HISP,black=BLACK,white=WHITE,pacific=PACIFIC,tr=TR,toteth=TOTETH)
data12$year <- 2013
#2014
file13 <- "~/Google Drive/CRPE/ccd/sc122a.txt" 
data13 <- read_delim(file13, delim = '\t')

data13 <- data13 %>%
  select(NCESSCH, FIPST,LEAID,SCHNO,STID,SEASCH,LEANM,SCHNAM,
         TYPE,STATUS,ULOCAL,LATCOD,LONCOD,LEVEL,MAGNET,CHARTR,TOTFRL,
         MEMBER,AM,ASIAN,HISP,BLACK,WHITE,PACIFIC,TR,TOTETH) %>%
  rename(ncessch=NCESSCH, fipst=FIPST,leaid=LEAID,schno=SCHNO,stid=STID,seasch=SEASCH,leanm=LEANM,schnam=SCHNAM,
         type=TYPE,status=STATUS,ulocal=ULOCAL,latcod=LATCOD,loncod=LONCOD,level=LEVEL,magnet=MAGNET,chartr=CHARTR,totfrl=TOTFRL,
         member=MEMBER,am=AM,asian=ASIAN,latino=HISP,black=BLACK,white=WHITE,pacific=PACIFIC,tr=TR,toteth=TOTETH)
data13$year <- 2014
###append and calculate proportions and other clean_up
ccd_2010_14 <- rbind(data09,data10, data11, data12, data13)
ccd_2010_14 <- data.frame(ccd_2010_14)
#Clean up missing values 
##fix missing values
ccd_2010_14[ ,]  <- lapply(ccd_2010_14[ ,] , 
                       FUN = function(x) {x[x == -1] <- NA; x})
ccd_2010_14[ ,]  <- lapply(ccd_2010_14[ ,] , 
                       FUN = function(x) {x[x == -2] <- NA; x})
ccd_2010_14[ ,]  <- lapply(ccd_2010_14[ ,] , 
                       FUN = function(x) {x[x == -9] <- NA; x})
#create proportions
ccd_2010_14 <- ccd_2010_14 %>%
                mutate(propblack = black/member,
                       proplatin = latino/member,
                       propasian = (asian+pacific)/member,
                       propwhite = white/member,
                       propfrl = totfrl/member
                       )
# Prep other variables
summary(ccd_2010_14$ulocal)
#Use these codes
#ULOCAL        24     AN     NCES urban-centric locale code.  
# 11 = City, Large
# 12 = City, Mid-size
# 13 = City, Small
# 21 = Suburb, Large
# 22 = Suburb, Mid-size
# 23 = Suburb, Small
# 31 = Town, Fringe
# 32 = Town, Distant
# 33 = Town, Remote
# 41 = Rural, Fringe
# 42 = Rural, Distant
# 43 = Rural, Remote


summary(ccd_2010_14$ulocal)


ccd_2010_14$urban <- ifelse(ccd_2010_14$ulocal == 11 | ccd_2010_14$ulocal == 12 | ccd_2010_14$ulocal == 13, "urban",
                         ifelse(is.na(ccd_2010_14$ulocal), "missing","not urban"))

ccd_2010_14$urban[is.na(ccd_2010_14$urban)] <- "missing"

table(ccd_2010_14$urban)
#########
#Use stata file to locate names created by Thiago using stata

ccd_2010_14$notschool <- tolower(ccd_2010_14$schnam)
# online home schools 
ccd_2010_14[grep("online | on-line",ccd_2010_14$notschool),]$notschool = "1"  

ccd_2010_14[grep("\\<distance\\>",ccd_2010_14$notschool),]$notschool = "1"  
ccd_2010_14[grep("home sch",ccd_2010_14$notschool),]$notschool = "1"  
ccd_2010_14[grep("\\<homelink\\>",ccd_2010_14$notschool),]$notschool = "1"  
ccd_2010_14[grep("\\<homebound\\>",ccd_2010_14$notschool),]$notschool = "1"  
ccd_2010_14[grep("\\<homeworks\\>",ccd_2010_14$notschool),]$notschool = "1"  
ccd_2010_14[grep("\\<home connections\\>",ccd_2010_14$notschool),]$notschool = "1"  
ccd_2010_14[grep("\\<virtual\\>",ccd_2010_14$notschool),]$notschool = "1"  
#juvinile detention/ hospital/detention centers 
ccd_2010_14[grep("det ctr",ccd_2010_14$notschool),]$notschool = "1" 
ccd_2010_14[grep("\\<juvenile\\>",ccd_2010_14$notschool),]$notschool = "1" 
#ccd_2010_14[grep("\\<correct\\>",ccd_2010_14$notschool),]$notschool = "1" 
ccd_2010_14[grep("\\<detention\\>",ccd_2010_14$notschool),]$notschool = "1" 
ccd_2010_14[grep("\\<j j a e p\\>",ccd_2010_14$notschool),]$notschool = "1" 
ccd_2010_14[grep("\\<jjaep\\>",ccd_2010_14$notschool),]$notschool = "1" 
ccd_2010_14[grep("\\<jail\\>",ccd_2010_14$notschool),]$notschool = "1" 
ccd_2010_14[grep("\\<treatment\\>",ccd_2010_14$notschool),]$notschool = "1" 
ccd_2010_14[grep("\\<hospital\\>",ccd_2010_14$notschool),]$notschool = "1" 
ccd_2010_14[grep("\\<hospitality\\>",ccd_2010_14$notschool),]$notschool = "1" 
ccd_2010_14[grep("\\<deaf\\>",ccd_2010_14$notschool),]$notschool = "1" 
ccd_2010_14[grep("\\<blind\\>",ccd_2010_14$notschool),]$notschool = "1" 
ccd_2010_14[grep("\\<therapy\\>",ccd_2010_14$notschool),]$notschool = "1" 
#ccd_2010_14[grep("facility",ccd_2010_14$notschool),]$notschool = "1" 
#$ccd_2010_14[grep("facilities",ccd_2010_14$notschool),]$notschool = "1" 
#ccd_2010_14[grep("transition",ccd_2010_14$notschool),]$notschool = "1" 
#special ed
ccd_2010_14[grep("\\<life skills\\>",ccd_2010_14$notschool),]$notschool = "1"
ccd_2010_14[grep("\\<skills center\\>",ccd_2010_14$notschool),]$notschool = "1"
ccd_2010_14[grep("\\<special\\>",ccd_2010_14$notschool),]$notschool = "1"
ccd_2010_14[grep("\\<sp ed\\>",ccd_2010_14$notschool),]$notschool = "1"
ccd_2010_14[grep("\\<spcl needs\\>",ccd_2010_14$notschool),]$notschool = "1"
ccd_2010_14[grep("independent stud",ccd_2010_14$notschool),]$notschool = "1"
ccd_2010_14[grep("\\<independent lrn\\>",ccd_2010_14$notschool),]$notschool = "1"
ccd_2010_14[grep("\\<independent learning\\>",ccd_2010_14$notschool),]$notschool = "1"
ccd_2010_14[grep("\\<independent alternative\\>",ccd_2010_14$notschool),]$notschool = "1" ##
ccd_2010_14[grep("\\<independent technical real access\\>",ccd_2010_14$notschool),]$notschool = "1"
#ccd_2010_14[grep("\\<continu\\>",ccd_2010_14$notschool),]$notschool = "1"
ccd_2010_14[grep("\\<alternative\\>",ccd_2010_14$notschool),]$notschool = "1" ##
ccd_2010_14[grep("\\<alt\\>",ccd_2010_14$notschool),]$notschool = "1"
ccd_2010_14[grep("\\<adult\\>",ccd_2010_14$notschool),]$notschool = "1"
ccd_2010_14[grep("\\<learning support\\>",ccd_2010_14$notschool),]$notschool = "1"

ccd_2010_14[grep("\\<lrn ctr\\>",ccd_2010_14$notschool),]$notschool = "1"
ccd_2010_14[grep("\\<lrn center\\>",ccd_2010_14$notschool),]$notschool = "1"
ccd_2010_14[grep("\\<learning ctr\\>",ccd_2010_14$notschool),]$notschool = "1"
ccd_2010_14[grep("\\<learning center\\>",ccd_2010_14$notschool),]$notschool = "1"

ccd_2010_14[grep("\\<program\\>",ccd_2010_14$notschool),]$notschool = "1"
ccd_2010_14[grep("\\<preschool\\>",ccd_2010_14$notschool),]$notschool = "1"
ccd_2010_14[grep("\\<nursery\\>",ccd_2010_14$notschool),]$notschool = "1"
#ccd_2010_14[grep("\\<renew acceler\\>",ccd_2010_14$notschool),]$notschool = "1"

ccd_2010_14[grep("tech. ctr.",ccd_2010_14$notschool),]$notschool = "1"
ccd_2010_14[grep("ctr.",ccd_2010_14$notschool),]$notschool = "1"
ccd_2010_14[grep("coop.",ccd_2010_14$notschool),]$notschool = "1"

ccd_2010_14$notschool <- ifelse(ccd_2010_14$notschool == "1" , "1","0")
table(ccd_2010_14$notschool)

#########Save file 
saveRDS(ccd_2010_14, file="~/Google Drive/CRPE/KauffmanKC/Data/ccd/ccd_2010_14.Rda")
#########