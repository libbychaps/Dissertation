#clear previous work
rm(list=ls())

#load packages
library(ggfortify)
library(ggplot2)
library(lme4)
library(lmerTest)
library(dplyr)
library(readxl)
library(tidyverse)
library(Hmisc)

#import dataset
sheep <- read_excel("~/University/4th Year/Dissertation/LibbyDataSet.xlsx")
View(sheep)

#calculating proportion successful in their first year
  summary(sheep$CountOfFirstRutOffspring) #shows that 1826 lambs did not sire offspring in 1st rut
  #so 221 males sired offspring in their first year

  propsired = (221/2047)
  #so proportion 0.1 (or 10%) of males sired lambs in their first year
  #is there a way of doing this more efficiently?

#count data for each trait
summary(sheep$MumID)
countmumID = 2047-269

summary(sheep$BirthYear)
countbirthyear = 2047

summary(sheep$PreciseBirthDate)
countbithdate = 2047-496

summary(sheep$CapAge)
countcapage = 2047-609

summary(sheep$BirthWt)
countbirthwt = 2047-611

summary(sheep$SibCount)
countsib = 2047

summary(sheep$Horn)
counthornty = 2047-2

summary(sheep$DeathYear)
countdeathyr = 2047

summary(sheep$PreciseDeathDate)
countdeathdat = 2047-1052

summary(sheep$CapYear)
countcapyr = 2047-1018

summary(sheep$PreciseCapDate)
countcapdat = 2047-1018

summary(sheep$Weight)
countwt = 2047-1022

summary(sheep$Foreleg)
countforeleg = 2047-1070

summary(sheep$Hindleg)
counthindleg = 2047-1049

summary(sheep$HornLen)
counthornlen = 2047-1040

summary(sheep$HornCirc)
counthorncirc = 2047-1088

summary(sheep$BolCirc)
countbolcirc = 2047-1188

summary(sheep$BolLen)
countbollen = 2047-1518

summary(sheep$CountOfFirstRutOffspring)
countfirstoff = 2047-1826

summary(sheep$LifetimeOffspring)
countlifetimeoff = 2047-1444

summary(sheep$VillTotal)
countvillpop = 2047

summary(sheep$VillTotalFemales)
countvillfem = 2047

summary(sheep$VillTotalMales)
countvillmale = 2047

summary(sheep$VillEweLamb)
countewelamb = 2047

summary(sheep$VillRamLamb)
countramlamb = 2047

#replace NA with 0 for CountOfFirstRutOffspring
sheep$CountOfFirstRutOffspring[is.na(sheep$CountOfFirstRutOffspring)] <- 0

#replace NA with 0 for LifetimeOffspring
sheep$LifetimeOffspring[is.na(sheep$LifetimeOffspring)]<- 0

#create column with mother known yes/no
sheep$MumKnown = sheep$MumID  #duplicates mumID column and names duplicate "MumKnown"
sheep$MumKnown[is.na(sheep$MumKnown)]<- 0 #assigns 0 to all NA values in MumKnown column
sheep$MumKnown[sheep$MumKnown > 0] <- 1   #assigns 1 to all >0 values in MumKnown column

#create column for sired offspring yes/no
sheep$success = sheep$CountOfFirstRutOffspring #duplicates CountOfFirstRutOffspring column
sheep$success[sheep$success >0]<- 1            #makes column binary 1/0

#calculate proportion sired each year
#1986
  y86<- sheep[sheep$BirthYear == 1986, ]  #subsets all data for sheep born in 1986
  View(y86)  
  success86<- length(y86$success[y86$success==1])  #counts all 1986 sheep that sired successfully 
  prop86 = (success86/16)
  prop86                            #0.5 sheep successful in 1986
#1987
  y87<- sheep[sheep$BirthYear == 1987, ] 
  View(y87)
  success87<- length(y87$success[y87$success == 1])
  prop87 = (success87/30)
  prop87                            #0.333 sheep successful in 1987
#1988
  y88<- sheep[sheep$BirthYear == 1988,]
  View(y88)
  success88<- length(y88$success[y88$success == 1])
  prop88 = (success88/12)
  prop88                            #0.083 sheep successful in 1988
#1989
  y89<- sheep[sheep$BirthYear == 1989,]
  View(y89)
  success89<- length(y89$success[y89$success == 1])
  prop89 = (success89/26)
  prop89                            #0.538 sheep successful in 1989
#1990
  y90<- sheep[sheep$BirthYear == 1990,]
  View(y90)
  success90<- length(y90$success[y90$success == 1])
  prop90 = (success90/38)
  prop90                            #0.237 sheep successful in 1990
#1991
  y91<- sheep[sheep$BirthYear == 1991,]
  View(y91)
  success91<- length(y91$success[y91$success == 1])
  prop91 = (success91/63)
  prop91                            #0.095 sheep successful in 1991
#1992
  y92<- sheep[sheep$BirthYear == 1992,]
  View(y92)
  success92<- length(y92$success[y92$success == 1])
  prop92 = (success92/31)
  prop92                            #0.194 sheep successful in 1992
#1993
  y93<- sheep[sheep$BirthYear == 1993,]
  View(y93)
  success93<- length(y93$success[y93$success == 1])
  prop93 = (success93/56)
  prop93                            #0.089 sheep successful in 1993
#1994
  y94<- sheep[sheep$BirthYear == 1994,]
  View(y94)
  success94<- length(y94$success[y94$success == 1])
  prop94= (success94/40)
  prop94                           #0.075 sheep successful in 1994
#1995
  y95<- sheep[sheep$BirthYear == 1995,]
  View(y95)
  success95<- length(y95$success[y95$success == 1])
  prop95 = (success95/56)
  prop95                           #0.268 sheep successful in 1995
#1996
  y96<- sheep[sheep$BirthYear == 1996,]
  View(y96)
  success96<- length(y96$success[y96$success == 1])
  prop96 = (success96/78)
  prop96                           #0.051 sheep successful in 1996
#1997
  y97<- sheep[sheep$BirthYear == 1997,]
  View(y97)
  success97<- length(y97$success[y97$success == 1])
  prop97 = (success97/70)
  prop97                           #0.029 sheep successful in 1997
#1998
  y98<- sheep[sheep$BirthYear == 1998,]
  View(y98)
  success98<- length(y98$success[y98$success == 1])
  prop98 = (success98/91)
  prop98                           #0.022 sheep successful in 1998
#1999
  y99<- sheep[sheep$BirthYear == 1999,]
  View(y99)
  success99<- length(y99$success[y99$success == 1])
  prop99 = (success99/70)
  prop99                           #0.214 sheep successful in 1999
#2000
  y00<- sheep[sheep$BirthYear == 2000,]
  View(y00)
  success00<- length(y00$success[y00$success == 1])
  prop00 = (success00/78)
  prop00                           #0.089 sheep successful in 2000
#2001
  y01<- sheep[sheep$BirthYear == 2001,]
  View(y01)
  success01<- length(y01$success[y01$success == 1])
  prop01 = (success01/88)
  prop01                           #0.034 sheep successful in 2001
#2002
  y02<- sheep[sheep$BirthYear == 2002,]
  View(y02)
  success02<- length(y02$success[y02$success == 1])
  prop02 = (success02/62)
  prop02                           #0.226 sheep successful in 2002
#2003
  y03<- sheep[sheep$BirthYear == 2003,]
  View(y03)
  success03<- length(y03$success[y03$success == 1])
  prop03 = (success03/93)
  prop03                           #0.118 sheep successful in 2003
#2004
  y04<- sheep[sheep$BirthYear == 2004,]
  View(y04)
  success04<- length(y04$success[y04$success == 1])
  prop04 = (success04/94)
  prop04                           #0.043 sheep successful in 2004
#2005
  y05<- sheep[sheep$BirthYear == 2005,]
  View(y05)
  success05<- length(y05$success[y05$success == 1])
  prop05 = (success05/48)
  prop05                           #0.083 sheep successful in 2005
#2006
  y06<- sheep[sheep$BirthYear == 2006,]
  View(y06)
  success06<- length(y06$success[y06$success == 1])
  prop06 = (success06/85)
  prop06                           #0.106 sheep successful in 2006
#2007
  y07<- sheep[sheep$BirthYear == 2007,]
  View(y07)
  success07<- length(y07$success[y07$success == 1])
  prop07 = (success07/75)
  prop07                           #0.173 sheep successful in 2007
#2008
  y08<- sheep[sheep$BirthYear == 2008,]
  View(y08)
  success08<- length(y08$success[y08$success == 1])
  prop08 = (success08/75)
  prop08                           #0.053 sheep successful in 2008
#2009
  y09<- sheep[sheep$BirthYear == 2009,]
  View(y09)
  success09<- length(y09$success[y09$success == 1])
  prop09 = (success09/88)
  prop09                           #0.022 sheep successful in 2009
#2010
  y10<- sheep[sheep$BirthYear == 2010,]
  View(y10)
  success10<- length(y10$success[y10$success == 1])
  prop10 = (success10/69)
  prop10                           #0.029 sheep successful in 2010
#2011
  y11<- sheep[sheep$BirthYear == 2011,]
  View(y11)
  success11<- length(y11$success[y11$success == 1])
  prop11 = (success11/122)
  prop11                           #0 sheep successful in 2011  <- WHAT HAPPENED IN 2011?
#2012
  y12<- sheep[sheep$BirthYear == 2012,]
  View(y12)
  success12<- length(y12$success[y12$success == 1])
  prop12 = (success12/48)
  prop12                           #0.354 sheep successful in 2012
#2013
  y13<- sheep[sheep$BirthYear == 2013,]
  View(y13)
  success13<- length(y13$success[y13$success == 1])
  prop13 = (success13/73)
  prop13                           #0.137 sheep successful in 2013
#2014
  y14<- sheep[sheep$BirthYear == 2014,]
  View(y14)
  success14<- length(y14$success[y14$success == 1])
  prop14 = (success14/72)
  prop14                           #0.111 sheep successful in 2014
#2015
  y15<- sheep[sheep$BirthYear == 2015,]
  View(y15)
  success15<- length(y15$success[y15$success == 1])
  prop15 = (success15/43)
  prop15                           #0.093 sheep successful in 2015
#2016
  y16<- sheep[sheep$BirthYear == 2016,]
  View(y16)
  success16<- length(y16$success[y16$success == 1])
  prop16 = (success16/63)
  prop16                           #0.063 sheep successful in 2016
#2017
  y17<- sheep[sheep$BirthYear == 2017,]
  View(y17)
  success17<- length(y17$success[y17$success == 1])
  prop17 = (success17/66)
  prop17                           #0.045 sheep successful in 2017
#2018
  y18<- sheep[sheep$BirthYear == 2018,]
  View(y18)
  success18<- length(y18$success[y18$success == 1])
  prop18 = (success18/28)
  prop18                           #0.071 sheep successful in 2018
  
#New dataframe for year factors (inc all year factors, made in excel)
  Years <- read_excel("~/University/4th Year/Dissertation/Years.xlsx")
  View(Years)

#add the above values into the years dataframe
YearPropVec <- c(prop86,prop87,prop88,prop89,prop90,prop91,prop92,prop93,prop94,prop95,
              prop96,prop97,prop98,prop99,prop00,prop01,prop02,prop03,prop04,prop05,prop06,
              prop07,prop08,prop09,prop10,prop11,prop12,prop13,prop14,prop15,prop16,prop17,prop18)
Years["YearProp"] <- YearPropVec

#create column for female:male ratio
Years<- Years %>% 
  mutate(ratio = VillTotalFemales/VillTotalMales)

#create column for adult females
Years<- Years %>%
  mutate(AdFemales = VillTotalFemales-VillEweLamb)

#create column for adult males
Years<- Years%>%
  mutate(AdMales = VillTotalMales-VillRamLamb)

#create column for adult female:male
Years<- Years%>%
  mutate(AdultRatio = AdFemales/AdMales)

#relationship between population and lamb success
plot(YearProp~VillTotal,data=Years)    
abline(lm(Years$YearProp~Years$VillTotal),col="red",lwd=3)   #less successful male lambs when pop higher
cor.test(Years$YearProp, Years$VillTotal, method="pearson")  #significant

#relationship between year and lamb success
plot(YearProp~Year,data=Years)
abline(lm(Years$YearProp~Years$Year),col="red",lwd=3)   #decrease over time
cor.test(Years$YearProp, Years$Year, method="pearson")  #significant

#relationship between total ratio and lamb success
plot(YearProp~ratio,data=Years)
abline(lm(Years$YearProp~Years$ratio),col="red",lwd=3)  #when there are more females:males, lambs are more successful
cor.test(Years$YearProp, Years$ratio, method="pearson") #significant

#relationship between adult ratio and lamb success
plot(YearProp~AdultRatio,data=Years)
abline(lm(Years$YearProp~Years$AdultRatio),col="red",lwd=3)
cor.test(Years$YearProp,Years$AdultRatio,method="pearson")

#relationship between no. of males and lamb success
plot(YearProp~VillTotalMales,data=Years)
abline(lm(Years$YearProp~Years$VillTotalMales),col="red",lwd=3)
cor.test(Years$YearProp, Years$VillTotalMales, method="pearson")

#relationship between adult ratio and total population
plot(VillTotal~AdultRatio,data=Years)
abline(lm(Years$VillTotal~Years$AdultRatio),col="red",lwd=3)
cor.test(Years$VillTotal,Years$AdultRatio,method="pearson")
#female:male ratio is correlated with population size
#mortality male biased so in small pops more females per male

#initial modelling
  #model for success (1/0)
  #create general linear mixed model with binomial data 
  plot(sheep$success~sheep$Weight)
  mod1<- glm(success~Weight,data=sheep,family=binomial)
  summary(mod1)

  plot(CountOfFirstRutOffspring~Weight,data=sheep)
  abline(lm(sheep$CountOfFirstRutOffspring~sheep$Weight),col="red",lwd=3)
  mod2<- glm(CountOfFirstRutOffspring~Weight,data=sheep)
  summary(mod2)
  #repro success skewed towards heavier individuals

  #looking at if males are immigrant or not (MumKnown as fixed effect)
  mod3<- glm(success~Weight+MumKnown,data=sheep,family=binomial)
  summary(mod3)
  #weight is significant, MumKnown is not - being an immigrant/not doesnt determine success

  mod4<- glmer(success~VillTotal+SibCount+Horn+(1|BirthYear),data=sheep,family=binomial)
  summary(mod4)
  #density, sib status and horn as fixed effects, birth year random

  #interaction between weight and horn type?
  mod5<- glm(success~Weight+Horn+Weight*Horn, data=sheep, family=binomial)
  summary(mod5)
  #no significant interaction between weight and horn type

  mod6<- glm(success~Weight+BolCirc+Weight*BolCirc,data=sheep,family=binomial)
  summary(mod6)
  #no significant interaction between weight and BolCirc

  mod7<- glm(success~Weight+BolLen+Weight*BolLen, data=sheep, family=binomial)
  summary(mod7)
  #no significant interaction between weight and BolLen

  mod8<- glm(success~Horn, data=sheep, family=binomial)
  summary(mod8)

#more correlations to test simple relationships (individual data)
plot(success~BolCirc,data=sheep)
abline(lm(sheep$success~sheep$BolCirc),col="red",lwd=3)
cor.test(sheep$success, sheep$BolCirc, method="pearson")

plot(success~BolLen,data=sheep)
abline(lm(sheep$success~sheep$BolLen),col="red",lwd=3)
cor.test(sheep$success, sheep$BolLen, method="pearson")

plot(CountOfFirstRutOffspring~BolLen,data=sheep)
abline(lm(sheep$CountOfFirstRutOffspring~sheep$BolLen),col="red",lwd=3)

plot(success~Foreleg,data=sheep)
abline(lm(sheep$success~sheep$Foreleg),col="red",lwd=3)
cor.test(sheep$success, sheep$Foreleg, method="pearson")

plot(success~Hindleg,data=sheep)
abline(lm(sheep$success~sheep$Hindleg),col="red",lwd=3)
cor.test(sheep$success, sheep$Hindleg, method="pearson")
#hindleg accounts for relationship (slightly) more than foreleg

#Use Hmisc package to get matrix of all correlations in Years data
rcorr(as.matrix(Years)) #this works for Years data 
rcorr(as.matrix(sheep)) #too many NAs for individual data?

#create column in sheep data for sex ratio
sheep<- sheep %>% 
  mutate(ratio = VillTotalFemales/VillTotalMales)

#Model for mother known (binary data)
  mod9<- glmer(success~MumKnown+VillTotal+ratio+(1|BirthYear),data=sheep,family=binomial)
  summary(mod9)
  #warning message says model is nearly unidentifiable???
  #ratio is not significant here
  mod9.2<- glmer(success~MumKnown+VillTotal+(1|BirthYear),data=sheep,family=binomial)
  summary(mod9.2)
  #MumKnown not significant
  mod9.3<- glmer(success~VillTotal+(1|BirthYear),data=sheep,family=binomial)
  summary(mod9.3)
  #try rescaling variables
  mod9.4<- glmer(success~scale(VillTotal)+(1|BirthYear),data=sheep,family=binomial)
  summary(mod9.4)#gets rid of error, changes result?
  #remove birth year
  mod9.5<- glm(success~scale(VillTotal),data=sheep,family=binomial)
  summary(mod9.5)
  
#Model for horn type and twin status (binary data)
  mod10<- glmer(success~SibCount+Horn+VillTotal+MumKnown+ratio+(1|BirthYear),data=sheep,family=binomial)
  summary(mod10)  #Horn least significant
  mod10.2<- glmer(success~SibCount+VillTotal+MumKnown+ratio+(1|BirthYear),data=sheep,family=binomial)
  summary(mod10.2)  #MumKnown least significant
  mod10.3<- glmer(success~SibCount+VillTotal+ratio+(1|BirthYear),data=sheep,family=binomial)
  summary(mod10.3)  #ratio least significant
  mod10.4<- glmer(success~SibCount+VillTotal+(1|BirthYear),data=sheep,family=binomial)
  summary(mod10.4)
  #remove (1|BirthYear)
  mod10.5<- glm(success~SibCount+VillTotal,data=sheep,family=binomial)
  summary(mod10.5)  
  
#Model for Aug catch animals (binary data)
  mod11<- glmer(success~Weight+Horn+HornLen+HornCirc+Hindleg+BolLen+BolCirc+SibCount+VillTotal+
                  ratio+(1|BirthYear),data=sheep,family=binomial)
  summary(mod11)   #none significant - remove terms one at a time (first HindLeg)
  mod11.2<- glmer(success~Weight+Horn+HornLen+HornCirc+BolLen+BolCirc+SibCount+VillTotal+
                  ratio+(1|BirthYear),data=sheep,family=binomial)
  summary(mod11.2) #Horn least significant
  mod11.3<- glmer(success~Weight+HornLen+HornCirc+BolLen+BolCirc+SibCount+VillTotal+
                  ratio+(1|BirthYear),data=sheep,family=binomial)
  summary(mod11.3) #BolLen least significant
  mod11.4<- glmer(success~Weight+HornLen+HornCirc+BolCirc+SibCount+VillTotal+
                  ratio+(1|BirthYear),data=sheep,family=binomial)
  summary(mod11.4) #VillTotal becomes significant, HornCirc least significant
  mod11.5<- glmer(success~Weight+HornLen+BolCirc+SibCount+VillTotal+
                  ratio+(1|BirthYear),data=sheep,family=binomial)
  summary(mod11.5) #BolCirc becomes significant, ratio least significant
  mod11.6<- glmer(success~Weight+HornLen+BolCirc+SibCount+VillTotal+
                  (1|BirthYear),data=sheep,family=binomial)
  summary(mod11.6) #HornLen least significant
  mod11.7<- glmer(success~Weight+BolCirc+SibCount+VillTotal+
                  (1|BirthYear),data=sheep,family=binomial)
  summary(mod11.7) #BolCirc no longer significant? SibCount least significant
  mod11.8<- glmer(success~Weight+BolCirc+VillTotal+(1|BirthYear),data=sheep,family=binomial)
  mod11.8.2<- glm(success~Weight+BolCirc+VillTotal,data=sheep,family=binomial) #removing random term gets rid of the warning message?
  summary(mod11.8) #weight is least significant
  summary(mod11.8.2) #weight also insignificant
  mod11.9<- glmer(success~BolCirc+VillTotal+(1|BirthYear),data=sheep,family=binomial) #warning
  mod11.9.2<- glm(success~BolCirc+VillTotal,data=sheep,family=binomial)
  summary(mod11.9.2) #all terms significant
  mod11.10<- glm(success~Weight+VillTotal,data=sheep,family=binomial) #remove BolCirc, keep weight
  summary(mod11.10) #makes weight significant
  
#Model for mother known (count data)
  mod12<- lm(CountOfFirstRutOffspring~MumKnown+VillTotal+ratio,data=sheep)
  summary(mod12)
  autoplot(mod12, smooth.colour = NA)  #this is not good
  hist(sheep$CountOfFirstRutOffspring) #violates normality
  #try logging data
  sheep<- cbind(sheep,log10(sheep$CountOfFirstRutOffspring))
  names(sheep)[names(sheep)=="log10(CountOfFirstRutOffspring)"] <- "logFirstRut"
  hist(sheep$`log10(sheep$CountOfFirstRutOffspring)`) #also not good
  #try square rooting data
  sheep<- cbind(sheep,sqrt(sheep$CountOfFirstRutOffspring))
  names(sheep)[names(sheep)=="sqrt(CountOfFirstRutOffspring)"] <- "sqrtFirstRut"
  hist(sheep$`sqrt(sheep$CountOfFirstRutOffspring)`) #also not good
  
  #try poisson distributed model (Jon's email)
  mod12.2<- glm(CountOfFirstRutOffspring~MumKnown+VillTotal+ratio,
                data=sheep,family=poisson)
  summary(mod12.2)  #this seems to work
  #remove MumKnown
  mod12.3<- glm(CountOfFirstRutOffspring~VillTotal+ratio,
                data=sheep,family=poisson)
  summary(mod12.3)
  #remove ratio
  mod12.4<- glm(CountOfFirstRutOffspring~VillTotal,
                data=sheep,family=poisson)
  summary(mod12.4)
  
#Model for horn type and twin status (count data)
  mod13<- glm(CountOfFirstRutOffspring~Horn+SibCount+VillTotal+MumKnown+ratio,
               data=sheep,family=poisson)
  summary(mod13) #remove horn type
  mod13.2<- glm(CountOfFirstRutOffspring~SibCount+VillTotal+MumKnown+ratio,
               data=sheep,family=poisson)
  summary(mod13.2) #remove ratio
  mod13.3<- glm(CountOfFirstRutOffspring~SibCount+VillTotal+MumKnown,
                data=sheep,family=poisson)
  summary(mod13.3) #remove MumKnown
  mod13.4<- glm(CountOfFirstRutOffspring~SibCount+VillTotal,
                data=sheep,family=poisson)
  summary(mod13.4) #SibCount and VillTotal both predict male lamb success
  
#Model for Aug catch animals (count data)
  mod14<- glm(CountOfFirstRutOffspring~Weight+Horn+HornLen+HornCirc+Hindleg+BolLen+BolCirc+SibCount+
                VillTotal+ratio,data=sheep,family=poisson)
  summary(mod14) #nothing significant in maximal model
  #remove Hindleg (least significant)
  mod14.2<- glm(CountOfFirstRutOffspring~Weight+Horn+HornLen+HornCirc+BolLen+BolCirc+SibCount+
                VillTotal+ratio,data=sheep,family=poisson)
  summary(mod14.2) #remove BolLen
  mod14.3<- glm(CountOfFirstRutOffspring~Weight+Horn+HornLen+HornCirc+BolCirc+SibCount+
                VillTotal+ratio,data=sheep,family=poisson)
  summary(mod14.3) #BolCirc and VillTotal become significant
  #remove SibCount
  mod14.4<- glm(CountOfFirstRutOffspring~Weight+Horn+HornLen+HornCirc+BolCirc+VillTotal,
                data=sheep,family=poisson)
  summary(mod14.4) #weight becomes significant
  #remove HornLen
  mod14.5<- glm(CountOfFirstRutOffspring~Weight+Horn+HornCirc+BolCirc+VillTotal,
                data=sheep,family=poisson)
  summary(mod14.5) #remove HornCirc
  mod14.5<- glm(CountOfFirstRutOffspring~Weight+Horn+BolCirc+VillTotal,
                data=sheep,family=poisson)
  summary(mod14.5) #remove weight
  mod14.6<- glm(CountOfFirstRutOffspring~Horn+BolCirc+VillTotal,
                data=sheep,family=poisson)
  summary(mod14.6) #remove horn
  mod14.7<- glm(CountOfFirstRutOffspring~BolCirc+VillTotal,
                data=sheep,family=poisson)
  summary(mod14.7) #minimal model
  #BolCirc and VillTotal influence number of offspring male lambs sire in their first year
  
  
#LOOKING AT FUTURE SURVIVAL AND REPRO
#correlations
  plot(DeathYear~success,data=sheep)    
  abline(lm(sheep$DeathYear~sheep$success),col="red",lwd=3)   
  cor.test(sheep$YearProp,sheep$VillTotal, method="pearson")
  
  plot(DeathYear~CountOfFirstRutOffspring,data=sheep)    
  abline(lm(sheep$DeathYear~sheep$CountOfFirstRutOffspring),col="red",lwd=3)   
  cor.test(sheep$DeathYear,sheep$CountOfFirstRutOffspring,method="pearson")
  
  plot(DeathYear~LifetimeOffspring,data=sheep)
  abline(lm(sheep$DeathYear~sheep$LifetimeOffspring),col="red",lwd=3)
  
  # ^^^ is looking at death year actually telling us anything? should look at death age
  #create column for age at death
  sheep<- sheep %>% 
    mutate(DeathAge = DeathYear-BirthYear)
  
  plot(DeathAge~success,data=sheep)    
  abline(lm(sheep$DeathAge~sheep$success),col="red",lwd=3)   
  cor.test(sheep$DeathAge,sheep$success,method="pearson")
  #sheep live slighty longer if they repro in first year
  
  plot(DeathAge~CountOfFirstRutOffspring,data=sheep)
  abline(lm(sheep$DeathAge~sheep$CountOfFirstRutOffspring),col="red",lwd=3)
  cor.test(sheep$DeathAge,sheep$CountOfFirstRutOffspring,method="pearson")
  #sheep live longer if they have more offspring in first year
  
  plot(DeathAge~LifetimeOffspring,data=sheep)
  abline(lm(sheep$DeathAge~sheep$LifetimeOffspring),col="red",lwd=3)
  cor.test(sheep$DeathAge,sheep$LifetimeOffspring,method="pearson")
  #sheep that love longer have more offspring - having offspring does not inc mortality
  
  #looking at future repro
  plot(LifetimeOffspring~success,data=sheep)
  abline(lm(sheep$LifetimeOffspring~sheep$success),col="red",lwd=3)  
  cor.test(sheep$LifetimeOffspring,sheep$success,method="pearson")  
  #significant but very small increase in lifetime offspring if successful in 1st year
  
  plot(LifetimeOffspring~CountOfFirstRutOffspring,data=sheep)
  abline(lm(sheep$LifetimeOffspring~sheep$CountOfFirstRutOffspring),col="red",lwd=3)
  cor.test(sheep$LifetimeOffspring,sheep$CountOfFirstRutOffspring,method="pearson")
  #increase in lifetime offspring if they had more offspring in first year
  
#lms
  
#model Death Age 
 hist(sheep$DeathAge) #not normally distributed --> poisson?
 mod15<- glmer(DeathAge~LifetimeOffspring+CountOfFirstRutOffspring+
                 Weight+LifetimeOffspring*CountOfFirstRutOffspring+
                 (1|BirthYear),data=sheep,family=poisson) 
 summary(mod13) #everything significant except first rut offspring 
 #add (1|DeathYear)
 mod15.2<- glmer(DeathAge~LifetimeOffspring+CountOfFirstRutOffspring+
                 Weight+LifetimeOffspring*CountOfFirstRutOffspring+
                 (1|BirthYear)+(1|DeathYear),data=sheep,family=poisson) 
 summary(mod15.2) #everything becomes insignificant except lifetime offspring (MAXIMAL MODEL)
 #remove (1|BirthYear)
 mod15.3<- glmer(DeathAge~LifetimeOffspring+CountOfFirstRutOffspring+
                 Weight+LifetimeOffspring*CountOfFirstRutOffspring+
                 (1|DeathYear),data=sheep,family=poisson)
 summary(mod15.3)  #weight becomes significant
 #remove first rut offspring from maximal model
 mod15.4<- glmer(DeathAge~LifetimeOffspring+Weight+
                   LifetimeOffspring*CountOfFirstRutOffspring+
                   (1|BirthYear)+(1|DeathYear),data=sheep,family=poisson)
 summary(mod15.4) #remove interaction
 mod15.5<- glmer(DeathAge~LifetimeOffspring+Weight+(1|BirthYear)+(1|DeathYear),
                 data=sheep,family=poisson)
 summary(mod15.5) #remove weight
 mod15.6<- glmer(DeathAge~LifetimeOffspring+(1|BirthYear)+(1|DeathYear),
                 data=sheep,family=poisson)
 summary(mod15.6) #becomes insignificant? go back to previous model or carry on?
 #try removing (1|BirthYear)
 mod15.7<- glmer(DeathAge~LifetimeOffspring+(1|DeathYear),
                 data=sheep,family=poisson)
 summary(mod15.7) #lifetime offspring becomes highly significant again
 
 
#model LBS
 hist(sheep$LifetimeOffspring) #poisson (or zero inflated?)
 mod16<- glmer(LifetimeOffspring~CountOfFirstRutOffspring+success+Weight+BolCirc+VillTotal+
                 (1|BirthYear)+(1|DeathYear),data=sheep,family=poisson)
 summary(mod16) #remove success
 mod16.2<- glmer(LifetimeOffspring~CountOfFirstRutOffspring+Weight+
                  (1|BirthYear)+(1|DeathYear),data=sheep,family=poisson)
 summary(mod16.2) #minimal model
 #CountOfFirstRutOffspring and Weight both affect LBS
 
 
###### plotting ######
#plot model 9.4
 mod9.4<- glmer(success~scale(VillTotal)+(1|BirthYear),data=sheep,family=binomial)
 summary(mod9.4)
 #plot in base R
 plot(success~VillTotal,data=sheep)
 abline(lm(sheep$success~sheep$VillTotal),col="red",lwd=3)
 cor.test(sheep$success,sheep$VillTotal,method="pearson")
 #plot using ggplot2
 plot1<- ggplot(sheep,aes(VillTotal,success))+             #creates base plot
     geom_point(aes(col=success))+                         #adds data points, diff colours for success/not
     labs(x="Total Village Population",y="First year breeding success")+     #adds labels to X and Y axes
     labs(color="Success")+theme_classic(base_size=18)+    #adds title to legend and changes background colour
     geom_smooth(method = "lm", se = FALSE,col="purple")   #adds line to graph
 plot1  #view plot
 