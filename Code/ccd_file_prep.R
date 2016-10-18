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
###append and save 
ccd_2010_14 <- rbind(data09,data10, data11, data12, data13)
saveRDS(ccd_2010_14, file="~/Google Drive/CRPE/ccd/ccd_2010_14.Rda")
#########