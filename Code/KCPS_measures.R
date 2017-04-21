# ARNOLD INDICATORS USING DISTRICT BOUNDARY
# ANALYSIS: Beating the odds, Stagnant, Gains, Top/bottom quintiles
# Jose Hernandez
# 10/20/2016

# (1) "Beating the odds" -- where look at the residuals of a relatively 
#     simple regression: 
#     %proficient ~ %white + %black + %hispanic + %frl + urban + schoollevel + schoolsize + e
#     Models are run separately by year. Schools are "beating the odds" if the 
#     residual, e, is negative (expected is lower than observed) and its 95% 
#     confidence interval of residual does not contain zero.
#     Reference: http://files.eric.ed.gov/fulltext/ED544802.pdf
# (2) "Stagnant measures" -- schools stuck in bottom 5% of state
# (3) "Gains" -- which are defined as the coefficient on a linear time trend (year)
#     in the following equation: %proficient ~ year + e
# (4) "Top/bottom quintiles" -- I flag schools in the bottom and top quintiles within
#     within each district, then find the proportion of students in each district (overall, 
#     and by FRL/non-FRL & white/black/latino) in those quintiles

### PREP THE DATA

rm(list=ls()) 

# Load packages
library(foreign)
library(plyr)
library(utils)
library(data.table)
## NOTE: need to restore old version of data.table (R was updated, and with it data.table, but old R version still on CSDE TS)
options(datatable.old.bywithoutby=TRUE)
library(dtplyr)
library(nlme)
library(dplyr)
library(tidyr)

# Set state-specific objects
## data file
data <- readRDS("~/Google Drive/CRPE/KauffmanKC/Data/KC2010_2014_city_district.rda")
str(data)

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

#change only certain columns to to numeric 

## years
year <- c(2010,2011,2012,2013,2014,2015,2016)
## cities
district <- "Kansas City 33 School District"


# Open & subset data file
###campture weird name schools 
table(data$level)

data$level[data$level=="N"] <- "4"
###tag non-schools 

data$notschool <- as.character(data$notschool)

data <- data[which(data$notschool== '0'),]

# Prep math & reading variables
data$math <- as.numeric(data$advPrf.Mathematics)


data$read <- as.numeric(data$advPrf.Eng..Language.Arts)


summary(data$math)
summary(data$read)
data$math <- data$math / 100
data$read <- data$read / 100

# Prep other variables

summary(data$level)
data$level[is.na(data$level)] <- 4

data$district <- as.character(as.character(data$district))
data$district.new <- ifelse(is.na(data$district), "Missing", data$district)

#asian numbers (don't agree with this JH)
summary(data$ENROLLMENT_ASIAN)
data$propasian <- rowSums(cbind(data$ENROLLMENT_ASIAN, data$ENROLLMENT_PACIFIC_ISLANDER), na.rm=T) / data$ENROLLMENT_GRADES_K_12

####################################################################
####################################################################

##### RUN TWICE:
##### 1st RUN = MATH
##### 2nd RUN = READ

data$subject <- data$read
#csvfilename <- "~/Google Drive/CRPE/KauffmanKC/analyses_4measures_OR_read.csv"

names(data)
####################################################################
### (1) BEAT THE ODDS

# Subset data
model <- data[,c("SCHOOL_NAME","subject","LUNCH_COUNT_FREE_REDUCTED_PCT","ENROLLMENT_WHITE_PCT","ENROLLMENT_BLACK_PCT",
                 "ENROLLMENT_HISPANIC_PCT","propasian","urban","level","ENROLLMENT_GRADES_K_12",
                 "YEAR","district.new","chartr")]

names(model) <- c("name","subject","propfrl","propwhite","propblack","prophisp",
                  "propasian","urban","level","enroll","year","district.new","ischarter")

mdata <- na.omit(model)
###Create data loop here...
########
data1 <- mdata[which(mdata$year==year[1]),]
data2 <- mdata[which(mdata$year==year[2]),]
data3 <- mdata[which(mdata$year==year[3]),]
data4 <- mdata[which(mdata$year==year[4]),]
data5 <- mdata[which(mdata$year==year[5]),]
data6 <- mdata[which(mdata$year==year[6]),]
data7 <- mdata[which(mdata$year==year[7]),]
# Run regressions by year, and save residuals
lm1 <- lm(subject ~ propfrl + propwhite + propblack + prophisp + propasian + factor(urban) + factor(level) + enroll, data=data1)
lm2 <- lm(subject ~ propfrl + propwhite + propblack + prophisp + propasian + factor(urban) + factor(level) + enroll, data=data2)
lm3 <- lm(subject ~ propfrl + propwhite + propblack + prophisp + propasian + factor(urban) + factor(level) + enroll, data=data3)
lm4 <- lm(subject ~ propfrl + propwhite + propblack + prophisp + propasian + factor(urban) + factor(level) + enroll, data=data4)
lm5 <- lm(subject ~ propfrl + propwhite + propblack + prophisp + propasian + factor(urban) + factor(level) + enroll, data=data5)
lm6 <- lm(subject ~ propfrl + propwhite + propblack + prophisp + propasian + factor(urban) + factor(level) + enroll, data=data6)
lm7 <- lm(subject ~ propfrl + propwhite + propblack + prophisp + propasian + factor(urban) + factor(level) + enroll, data=data7)

# Calculate residuals' 90% confidence intervals
data1$res <- residuals(lm1)
data1$res.lo <- data1$res - (1.645*sd(data1$res))
data1$res.hi <- data1$res + (1.645*sd(data1$res))
data2$res <- residuals(lm2)
data2$res.lo <- data2$res - (1.645*sd(data2$res))
data2$res.hi <- data2$res + (1.645*sd(data2$res))
data3$res <- residuals(lm3)
data3$res.lo <- data3$res - (1.645*sd(data3$res))
data3$res.hi <- data3$res + (1.645*sd(data3$res))
#
data4$res <- residuals(lm4)
data4$res.lo <- data4$res - (1.645*sd(data4$res))
data4$res.hi <- data4$res + (1.645*sd(data4$res))
#
data5$res <- residuals(lm5)
data5$res.lo <- data5$res - (1.645*sd(data5$res))
data5$res.hi <- data5$res + (1.645*sd(data5$res))
#
data6$res <- residuals(lm6)
data6$res.lo <- data6$res - (1.645*sd(data6$res))
data6$res.hi <- data6$res + (1.645*sd(data6$res))
#
data7$res <- residuals(lm7)
data7$res.lo <- data7$res - (1.645*sd(data7$res))
data7$res.hi <- data7$res + (1.645*sd(data7$res))

# Identify schools that beat the odds
data1$beat <- ifelse(data1$res>0 & data1$res.lo>0, 1, 
                     ifelse(data1$res<=0 | data1$res.lo<=0, 0, NA))
data2$beat <- ifelse(data2$res>0 & data2$res.lo>0, 1, 
                     ifelse(data2$res<=0 | data2$res.lo<=0, 0, NA))
data3$beat <- ifelse(data3$res>0 & data3$res.lo>0, 1, 
                     ifelse(data3$res<=0 | data3$res.lo<=0, 0, NA))
#
data4$beat <- ifelse(data4$res>0 & data4$res.lo>0, 1, 
                     ifelse(data4$res<=0 | data4$res.lo<=0, 0, NA))
#
data5$beat <- ifelse(data5$res>0 & data5$res.lo>0, 1, 
                     ifelse(data5$res<=0 | data5$res.lo<=0, 0, NA))
#
data6$beat <- ifelse(data6$res>0 & data6$res.lo>0, 1, 
                     ifelse(data6$res<=0 | data6$res.lo<=0, 0, NA))
#
data7$beat <- ifelse(data7$res>0 & data7$res.lo>0, 1, 
                     ifelse(data7$res<=0 | data7$res.lo<=0, 0, NA))

# Keep only necessary variables
myvars <- c("district.new", "year", "subject", "beat", "ischarter", "enroll")
data1 <- data1[myvars]
data2 <- data2[myvars]
data3 <- data3[myvars]
data4 <- data4[myvars]
data5 <- data5[myvars]
data6 <- data6[myvars]
data7 <- data7[myvars]
# Append years together
master <- rbind(data1, data2, data3, data4, data5, data6, data7)

# Keep only cities of interest
master <- master[which(master$district.new %in% district),]

# Create categorical time variable
master$year <- master$year - min(master$year) + 1
master$time[master$year==1] <- "Y1"
master$time[master$year==2] <- "Y2"
master$time[master$year==3] <- "Y3"
master$time[master$year==4] <- "Y4"
master$time[master$year==5] <- "Y5"
master$time[master$year==6] <- "Y6"
master$time[master$year==7] <- "Y7"

# Create categorical sector variable
master$sector[master$ischarter==0 | master$ischarter==2] <- "tps"
master$sector[master$ischarter==1] <- "cs"

# Aggregate master data: find % of all schools and students beating the odds
master$temp <- 1
master.agg <- ddply(master, .(district.new, time), function(x)
  c(enrolltotal = sum(x$enroll),
    enrollbeat  = sum(x[which(x$beat==1),]$enroll),
    schooltotal = sum(x$temp),
    schoolbeat  = sum(x[which(x$beat==1),]$temp) ) )
master.agg$enrollbeat.pct = master.agg$enrollbeat / master.agg$enrolltotal
master.agg$schoolbeat.pct = master.agg$schoolbeat / master.agg$schooltotal

# Aggregate master data: find % of schools and students beating the odds, by sector
master$temp <- 1
master.sec <- ddply(master, .(district.new, time, sector), function(x)
  c(enrolltotal = sum(x$enroll),
    enrollbeat  = sum(x[which(x$beat==1),]$enroll),
    schooltotal = sum(x$temp),
    schoolbeat  = sum(x[which(x$beat==1),]$temp) ) )
master.sec$enrollbeat.pct = master.sec$enrollbeat / master.sec$enrolltotal
master.sec$schoolbeat.pct = master.sec$schoolbeat / master.sec$schooltotal

master.sec2 <- reshape(master.sec, 
                       timevar=c("sector"), 
                       idvar=c("district.new", "time"), 
                       direction="wide")

# Merge the two aggregated dataframes
master2 <- merge(master.agg, master.sec2, by=c("district.new", "time"))

write.csv(master2, file="~/Google Drive/CRPE/KauffmanKC/btomaster.csv")
saveRDS(master2, file="~/Google Drive/CRPE/KauffmanKC/btomaster.Rdata")

xtable(master.agg)
print(xtable(master.agg[,-1],digits=c(0,0,0,0,0,0,2,2), caption = 'BTO Results Overall'), include.rownames=F,caption.placement = 'top')

print(xtable(master.sec[,-1],digits=c(0,0,0,0,0,0,0,2,2), caption = 'BTO Results by Sector'), include.rownames=F,caption.placement = 'top')

# Reshape (long -> wide)
#bto <- reshape(master2, timevar="time", idvar="district.new", direction="wide")
#names(bto)[1] <- "district"
####################################################################

### (2) STAGNANCY MEASURE

myvars <- c("YEAR", "seasch", "district.new", "ENROLLMENT_GRADES_K_12", "subject")

stdata <- data[myvars]
names(stdata) <- c("year", "seasch", "district.new", "enroll", "subject")

stdata <- na.omit(stdata)
summary(stdata)

# Find cutoffs for bottom 5% of state
pct5a <- quantile(stdata$subject[stdata$year==year[1]], c(0.05))
pct5b <- quantile(stdata$subject[stdata$year==year[2]], c(0.05))
pct5c <- quantile(stdata$subject[stdata$year==year[3]], c(0.05))
pct5d <- quantile(stdata$subject[stdata$year==year[4]], c(0.05))
pct5e <- quantile(stdata$subject[stdata$year==year[5]], c(0.05))
pct5f <- quantile(stdata$subject[stdata$year==year[6]], c(0.05))
pct5g <- quantile(stdata$subject[stdata$year==year[7]], c(0.05))

# Mark which schools are in bottom 5% of state
stdata$stuck <- NA
stdata$stuck[stdata$year==year[1]] <- ifelse(stdata$subject[stdata$year==year[1]]<=pct5a, 1, 0)
stdata$stuck[stdata$year==year[2]] <- ifelse(stdata$subject[stdata$year==year[2]]<=pct5b, 1, 0)
stdata$stuck[stdata$year==year[3]] <- ifelse(stdata$subject[stdata$year==year[3]]<=pct5c, 1, 0)
stdata$stuck[stdata$year==year[4]] <- ifelse(stdata$subject[stdata$year==year[4]]<=pct5d, 1, 0)
stdata$stuck[stdata$year==year[5]] <- ifelse(stdata$subject[stdata$year==year[5]]<=pct5e, 1, 0)
stdata$stuck[stdata$year==year[6]] <- ifelse(stdata$subject[stdata$year==year[6]]<=pct5f, 1, 0)
stdata$stuck[stdata$year==year[7]] <- ifelse(stdata$subject[stdata$year==year[7]]<=pct5g, 1, 0)
# Set cities and time variable
stdata <- stdata[which(stdata$district.new==district[1]), ]

stdata$year <- stdata$year - min(stdata$year) + 1
stdata$time[stdata$year==1] <- "Y1"
stdata$time[stdata$year==2] <- "Y2"
stdata$time[stdata$year==3] <- "Y3"
stdata$time[stdata$year==4] <- "Y4"
stdata$time[stdata$year==5] <- "Y5"
stdata$time[stdata$year==6] <- "Y6"
stdata$time[stdata$year==7] <- "Y7"

# Create variable to count schools
stdata$temp <- 1

# Find number of students & schools, overall & in bottom 5% of state
stag <- ddply(stdata, .(district.new, time), function(x)
  c(enrolltotal=sum(x$enroll),
    enrollstuck=sum(x[which(x$stuck==1),]$enroll),
    schooltotal=sum(x$temp),
    schoolstuck=sum(x[which(x$stuck==1),]$temp) ) )

# Calculate percent of students & schools in bottom 5% of state
stag$stuck.stu <- stag$enrollstuck / stag$enrolltotal
stag$stuck.sch <- stag$schoolstuck / stag$schooltotal

# Create dataframe of above results
vars <- c("district.new", "time", "stuck.stu", "stuck.sch")
stag <- stag[vars]

stag2 <- reshape(stag, timevar=c("time"), idvar=c("district.new"), direction="wide")

print(xtable(stag[,-1],digits=c(0,0,2,2), caption = 'Proportion of Schools and Children in the Bottom 5% in Math'), include.rownames=F,caption.placement = 'top')

# Find share of schools in Y1 that were stuck for all 3 years
## mark if stuck in y1
stdata$iny1 <- NA
stdata$iny1 <- ifelse(stdata$year==1 & stdata$stuck==1, 1, 0)
## collapse by school
change <- ddply(stdata, .(seasch, district.new), function(x)
  c(stuckinyr1=sum(x$iny1),
    stucktotal=sum(x$stuck) ) )
summary(change) #27.66% stuck in year 1 
## subset to include only schools stuck in y1
change <- change[which(change$stuckinyr1==1),]
##mark if stuck in all 3 years 
change$all3 <- NA
change$all3 <- ifelse(change$stucktotal==3, 1, 0)
## mark if stuck in all 7 years
change$all7 <- NA
change$all7 <- ifelse(change$stucktotal==7, 1, 0)
## collapse by district
change2 <- ddply(change, .(district.new), function(x)
  c(stuck.sch.y1=sum(x$stuckinyr1),
    stuck.sch.all3=sum(x$all3),
    stuck.sch.all7=sum(x$all7)) )
## calculate percentage of schools stuck in y1 that were stuck in all 7 years
change2$stuck.sch.y1all7 <- change2$stuck.sch.all7 / change2$stuck.sch.y1
change2$stuck.sch.y1all3 <- change2$stuck.sch.all3 / change2$stuck.sch.y1
change2$stuck.sch.y1 <- 0.383
## reduce dataframe
vars <- c("district.new", "stuck.sch.y1","stuck.sch.y1all3","stuck.sch.y1all7")
change2 <- change2[vars]

print(xtable(change2[,-1],digits=c(2,2,2,2), caption = ' Persistence of Schools in the Bottom 5 Percent From Year 1 to Year 7 in Math'), include.rownames=F,caption.placement = 'top')

# Merge the two dataframes
stagnant <- merge(stag, change2, by="district.new")
names(stagnant)[names(stagnant)=="district.new"] <- "district"

####################################################################
### (3) GAINS
## Jose: Used lmer specification to look at gains
##Looked at gains in terms of the school average by year and their deviation from the overall average (+ or - ) 


## Adjusted gains measure with clustered standard errors (Chingos review, 5/4/15, + convo w/ BG, 5/6/15)
## NOTE: this measure replaces both the original (unadjusted) HLM model and the adjusted (non-HLM) model
#install.packages('plm')
#install.packages('lmtest')

library(plm)
library(lmtest)
install.packages("multiwayvcov")
library(lme4)
#install.packages("arm")
library(arm)
get_confint<-function(model, vcovCL){
  t<-qt(.975, model$df.residual)
  ct<-coeftest(model, vcovCL)
  est<-cbind(ct[,1], ct[,1]-t*ct[,2], ct[,1]+t*ct[,2])
  colnames(est)<-c("Estimate","LowerCI","UpperCI")
  return(est)
}

cluster.se <- function(model, cluster) {
  require(multiwayvcov)
  require(lmtest)
  vcovCL<-cluster.vcov(model, cluster)
  
  coef<-coeftest(model, vcovCL)
  w<-waldtest(model, vcov = vcovCL, test = "F")
  ci<-get_confint(model, vcovCL)
  
  return(list(coef, w, ci))
}

data$seasch <- as.numeric(as.character(data$seasch))

model <- subject ~ year + district.new + seasch + propfrl + propwhite + propblack + prophisp + propasian + factor(urban) + factor(level) + enroll

model <- data[,c("seasch","subject","LUNCH_COUNT_FREE_REDUCTED_PCT","ENROLLMENT_WHITE_PCT","ENROLLMENT_BLACK_PCT",
                 "ENROLLMENT_HISPANIC_PCT","propasian","urban","level","ENROLLMENT_GRADES_K_12",
                 "YEAR","district.new","chartr")]

names(model) <- c("seasch","subject","propfrl","propwhite","propblack","prophisp",
                  "propasian","urban","level","enroll","year","district.new","ischarter")

mdata <- na.omit(model)
mdata$time <- mdata$year - min(mdata$year)

mdata <- data.table(mdata, key="year")
mdata[,subject.std:=scale(subject),by=year]

#data1 <- mdata[which(mdata$district.new==district[1]),]
#compared the fixed effects speciofication of time by using "factor" 
#if you use these function you have to take out factor

#equation <- subject.std ~ as.factor(time) + propfrl + propwhite + propblack + prophisp + propasian + factor(level) + enroll

#Use:
#equation <- subject.std ~ time + propfrl + propwhite + propblack + prophisp + propasian + factor(level) + enroll
#lm1 <- lm(equation, data=data1)

#se1 <- cluster.se(lm1, data1$seasch)


#b1.adj <- se1[[1]][2,1]
#p1.adj <- se1[[1]][2,4]



#gains.b.adj <- c(b1.adj) #beta for year 
#gains.p.adj <- c(p1.adj) #p-value
#
######
#fixed effects specification
#MLexamp.3 <- glm(equation, data = data1)
#display(MLexamp.3)
#AIC(MLexamp.3)
#lme
#MLexamp.6 <- lmer(subject.std ~ time + propfrl + propwhite + propblack + prophisp + propasian + factor(level) + enroll + (1 | seasch), data = data1)
#display(MLexamp.6)

#fm06 <- lmer(subject.std ~ time + propfrl + propwhite + propblack + prophisp + propasian + factor(level) + enroll + (1 + time|seasch), data1,REML=FALSE)
#display(fm06)
#summary(fm06)
#head(ranef(fm06)[["seasch"]])
####
#model.b <- lmer(subject.std ~ time + (1 + time|seasch), data1,REML=FALSE)
#display(model.b)
#########
#install.packages("nlme")
library(nlme)

#model.b <- lme(subject.std ~ time, data=data1, random= ~ time | seasch, method="ML")
#summary(model.b)


#fixef.b <- fixef(model.b)


#fit.b <- fixef.b[[1]] + seq(0, 6)*fixef.b[[2]]
#x <- 2010:2016
#plot(x, fit.b, ylim=c(-1, -.6), type="b", 
#     ylab="predicted gains in SD Units", xlab="Year")   
#title("Model B \n Unconditional growth model")

##USED THIS MODEL for gains with more than 3 years of data
model.c <- lme(subject.std ~ time + propfrl + propwhite + propblack + prophisp + propasian + factor(level) + enroll, data=data1, random= ~ time | seasch, method="ML")
summary(model.c)


fixef.c <- fixef(model.c)


fit.c <- fixef.c[[1]] + seq(0, 6)*fixef.c[[2]]
x <- 2010:2016
plot(x, fit.c, ylim=c(-2, -1.6), type="b", 
     ylab="predicted gains in SD Units", xlab="Year")   
title("FE growth model")

####################################################################
### (4) TOP/BOTTOM QUINTILES

myvars <- c("YEAR", "seasch", "district.new", "level", "subject", "ENROLLMENT_GRADES_K_12", "LUNCH_COUNT_FREE_REDUCED", 
            "ENROLLMENT_WHITE", "ENROLLMENT_BLACK", "ENROLLMENT_HISPANIC")
qdata <- data[myvars]

names(qdata) <- c("year", "seasch", "district.new", "level", "subject", "enroll", "numfrl", "numwhite", "numblack", "numhisp")

qdata$schlevel[qdata$level==1 | qdata$level==2] <- "ElemMid"
qdata$schlevel[qdata$level==3] <- "High"
qdata$schlevel[qdata$level==4] <- NA

qdata$year <- qdata$year - min(qdata$year) + 1
qdata$time[qdata$year==1] <- "Y1"
qdata$time[qdata$year==2] <- "Y2"
qdata$time[qdata$year==3] <- "Y3"
qdata$time[qdata$year==4] <- "Y4"
qdata$time[qdata$year==5] <- "Y5"
qdata$time[qdata$year==6] <- "Y6"
qdata$time[qdata$year==7] <- "Y7"

qdata <- na.omit(qdata)

qdata <- qdata[which(qdata$district.new==district[1]), ]

qdata$numnonfrl <- qdata$enroll - qdata$numfrl

quint <- ddply(qdata, .(district.new, time, schlevel), function(x) 
  c(p20=quantile(x$subject, c(0.20)),
    p80=quantile(x$subject, c(0.80)),
    enroll=sum(x$enroll),
    numfrl=sum(x$numfrl),
    numnonfrl=sum(x$numnonfrl),
    numwhite=sum(x$numwhite),
    numblack=sum(x$numblack),
    numhisp=sum(x$numhisp),
    p20.enroll=sum(x[which(x$subject<=quantile(x$subject, c(0.20))),]$enroll),
    p80.enroll=sum(x[which(x$subject>=quantile(x$subject, c(0.80))),]$enroll), 
    p20.frl=sum(x[which(x$subject<=quantile(x$subject, c(0.20))),]$numfrl),
    p80.frl=sum(x[which(x$subject>=quantile(x$subject, c(0.80))),]$numfrl), 
    p20.nonfrl=sum(x[which(x$subject<=quantile(x$subject, c(0.20))),]$numnonfrl),
    p80.nonfrl=sum(x[which(x$subject>=quantile(x$subject, c(0.80))),]$numnonfrl), 
    p20.white=sum(x[which(x$subject<=quantile(x$subject, c(0.20))),]$numwhite),
    p80.white=sum(x[which(x$subject>=quantile(x$subject, c(0.80))),]$numwhite), 
    p20.black=sum(x[which(x$subject<=quantile(x$subject, c(0.20))),]$numblack),
    p80.black=sum(x[which(x$subject>=quantile(x$subject, c(0.80))),]$numblack), 
    p20.hisp=sum(x[which(x$subject<=quantile(x$subject, c(0.20))),]$numhisp),
    p80.hisp=sum(x[which(x$subject>=quantile(x$subject, c(0.80))),]$numhisp) ) )


colnames(quint)[1] <- "district"
colnames(quint)[4] <- "p20"
colnames(quint)[5] <- "p80"

quint$diff8020 <- round((quint$p80 - quint$p20), digits=2)
quint$ratio8020 <- round((quint$p80 / quint$p20), digits=2)

quint$p20.enroll <- round((quint$p20.enroll / quint$enroll), digits=4)*100
quint$p80.enroll <- round((quint$p80.enroll / quint$enroll), digits=4)*100
quint$p20.frl <- round((quint$p20.frl / quint$numfrl), digits=4)*100
quint$p80.frl <- round((quint$p80.frl / quint$numfrl), digits=4)*100
quint$p20.nonfrl <- round((quint$p20.nonfrl / quint$numnonfrl), digits=4)*100
quint$p80.nonfrl <- round((quint$p80.nonfrl / quint$numnonfrl), digits=4)*100
quint$p20.white <- round((quint$p20.white / quint$numwhite), digits=4)*100
quint$p80.white <- round((quint$p80.white / quint$numwhite), digits=4)*100
quint$p20.black <- round((quint$p20.black / quint$numblack), digits=4)*100
quint$p80.black <- round((quint$p80.black / quint$numblack), digits=4)*100
quint$p20.hisp <- round((quint$p20.hisp / quint$numhisp), digits=4)*100
quint$p80.hisp <- round((quint$p80.hisp / quint$numhisp), digits=4)*100

vars <- c("district", "time", "schlevel", "p20", "p80", "p20.enroll",
          "p80.enroll", "p20.frl", "p80.frl", "p20.nonfrl",
          "p80.nonfrl", "p20.white", "p80.white", "p20.black", 
          "p80.black", "p20.hisp", "p80.hisp", "diff8020", "ratio8020")
quint <- quint[vars]

non <- quint[,1:3]

tab1 <- quint[,1:11]
tab2 <- quint[,12:19]
tab2 <- cbind(non,tab2)

print(xtable(tab1[,-1],digits=c(0,0,2,2,2,2,2,2,2,2,2), caption = 'Enrollment in High and Low-Scoring Elementary and Middle Schools'), include.rownames=F,caption.placement = 'top')
print(xtable(tab2[,-1],digits=c(0,0,2,2,2,2,2,2,2,2,2), caption = 'Enrollment in High and Low-Scoring Elementary and Middle Schools (Cont.)'), include.rownames=F,caption.placement = 'top')

#write.csv(quint, file="...")
#####

quint2 <- reshape(quint,
                  timevar=c("time"),
                  idvar=c("district", "schlevel"),
                  direction="wide")
quint2 <- reshape(quint2,
                  timevar=c("schlevel"),
                  idvar=c("district"),
                  direction="wide")

