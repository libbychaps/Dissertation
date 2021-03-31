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
library(performance)
library(patchwork)
library(scatterplot3d)
library(plotly)
library(see)
library(qqplotr)
library(RColorBrewer)
library(viridis)
library(scales)
library(plotrix)

#import dataset
sheep <- read_excel("~/University/4th Year/Dissertation/LibbyDataSet.xlsx")
View(sheep)
str(sheep)

### 1. Summary statistics and df manipulation ###

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

#-------------------------------------------------------------------------------
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
#---------------------------------------------------------------------------------------------
  
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
  
  #create column in sheep data for sex ratio
  sheep<- sheep %>% 
    mutate(ratio = VillTotalFemales/VillTotalMales)

  
### 2. Basic correlations (first year success) ###
  #population and ram lamb success
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
  
  
### 3. Initial modelling (First year success) ###
  #model for success (1/0)
  #create generalised linear model with binomial data 
  mod1<- glm(success~Weight,data=sheep,family=binomial)
  summary(mod1)

  mod2<- glm(CountOfFirstRutOffspring~Weight,data=sheep)
  summary(mod2)
  #repro success skewed towards heavier individuals

  #looking at if males are immigrant or not (MumKnown as fixed effect)
  mod3<- glm(success~Weight+MumKnown,data=sheep,family=binomial)
  summary(mod3)
  #weight is significant, MumKnown is not - being an immigrant/not doesn't determine success

  mod4<- glmer(success~VillTotal+SibCount+Horn+(1|BirthYear),data=sheep,family=binomial)
  summary(mod4)
  mod4.2<- glmer(success~VillTotal+SibCount+(1|BirthYear),data=sheep,family=binomial)
  summary(mod4.2)
  mod4.3<- glm(success~VillTotal+SibCount,data=sheep,family=binomial)
  summary(mod4.3)
  
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
  
  #hindleg and weight
  plot(Hindleg~Weight,data=sheep)
  abline(lm(sheep$Hindleg~sheep$Weight),col="red",lwd=3)
  cor.test(sheep$Hindleg,sheep$Weight,method="pearson")
  
  #BolCirc and Weight
  plot(BolCirc~Weight,data=sheep)
  abline(lm(sheep$BolCirc~sheep$Weight),col="red",lwd=3) 
  cor.test(sheep$Hindleg,sheep$Weight,method="pearson") 
  
  #Birth weight and Aug weight
  plot(BirthWt~Weight,data=sheep)
  abline(lm(sheep$BirthWt~sheep$Weight),col="red",lwd=3) 
  
  #BolCirc and LBS
  plot(LifetimeOffspring~BolCirc,data=sheep)
  abline(lm(sheep$LifetimeOffspring~sheep$BolCirc),col="red",lwd=3) 
  
  #BolCirc and 1st year success
  plot(success~BolCirc,data=sheep)
  abline(lm(sheep$success~sheep$BolCirc),col="red",lwd=3) 
  
  #LBS and success
  plot(LifetimeOffspring~success,data=sheep)
  abline(lm(sheep$LifetimeOffspring~sheep$success),col="red",lwd=3) 
  #the most successful individuals did not have offspring in their first year 
  
  #BolCirc and Hindleg
  plot(BolCirc~Hindleg,data=sheep)
  abline(lm(sheep$BolCirc~sheep$Hindleg),col="red",lwd=3) 


### 4. Final models (First year success) ###
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
    mod9.6<- glm(success~VillTotal,data=sheep,family=binomial)
    summary(mod9.6)
    #check model performance
    model_performance(mod9.5) #unsure what this tells us
    #check r2
    r2(mod9.5)
    check_model(mod9.5)
    
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
    check_model(mod10.5)
    
  #Model for Aug catch animals (binary data)
    mod11<- glmer(success~Weight+Horn+HornLen+HornCirc+Hindleg+BolLen+BolCirc+SibCount+VillTotal+
                    ratio+Weight*VillTotal+(1|BirthYear),data=sheep,family=binomial)
    summary(mod11)   #none significant - remove terms one at a time (first HindLeg)
    mod11.2<- glmer(success~Weight+Horn+HornLen+HornCirc+BolLen+BolCirc+SibCount+VillTotal+
                    ratio+Weight*VillTotal+(1|BirthYear),data=sheep,family=binomial)
    summary(mod11.2) #Horn least significant
    mod11.3<- glmer(success~Weight+HornLen+HornCirc+BolLen+BolCirc+SibCount+VillTotal+
                    ratio+Weight*VillTotal+(1|BirthYear),data=sheep,family=binomial)
    summary(mod11.3) #BolLen least significant
    mod11.4<- glmer(success~Weight+HornLen+HornCirc+BolCirc+SibCount+VillTotal+
                    ratio+Weight*VillTotal+(1|BirthYear),data=sheep,family=binomial)
    summary(mod11.4) #HornCirc least significant
    mod11.5<- glmer(success~Weight+HornLen+BolCirc+SibCount+VillTotal+
                    ratio+Weight*VillTotal+(1|BirthYear),data=sheep,family=binomial)
    summary(mod11.5) #BolCirc becomes significant, Weight least significant
    mod11.6<- glmer(success~HornLen+BolCirc+SibCount+VillTotal+ratio+
                    Weight*VillTotal+(1|BirthYear),data=sheep,family=binomial)
    summary(mod11.6) #remove ratio
    mod11.7<- glmer(success~HornLen+BolCirc+SibCount+VillTotal+Weight+Weight*VillTotal+
                      (1|BirthYear),data=sheep,family=binomial)
    summary(mod11.7) #remove HornLen
    mod11.8<- glmer(success~BolCirc+SibCount+VillTotal+Weight+Weight*VillTotal+
                      (1|BirthYear),data=sheep,family=binomial)
    summary(mod11.8) #remove SibCount
    mod11.9<- glmer(success~BolCirc+VillTotal+Weight+Weight*VillTotal+
                      (1|BirthYear),data=sheep,family=binomial)
    summary(mod11.9) #remove interaction
    mod11.10<- glmer(success~BolCirc+VillTotal+Weight+(1|BirthYear),data=sheep,family=binomial)
    summary(mod11.10)   #remove weight
    mod11.11<- glmer(success~BolCirc+VillTotal+(1|BirthYear),data=sheep,family=binomial)
    summary(mod11.11) #remove (1|BirthYear)
    mod11.12<- glm(success~BolCirc+VillTotal,data=sheep,family=binomial)
    summary(mod11.12)
    #including an interaction does not change minimal model
    
    #repeat this model using an interaction between VillTotal and BolCirc
    mod19<- glmer(success~Weight+Horn+HornLen+HornCirc+Hindleg+BolLen+BolCirc+SibCount+VillTotal+
                    ratio+BolCirc*VillTotal+(1|BirthYear),data=sheep,family=binomial)
    summary(mod19) #remove Hindleg
    mod19.2<- glmer(success~Weight+Horn+HornLen+HornCirc+BolLen+BolCirc+SibCount+VillTotal+
                    ratio+BolCirc*VillTotal+(1|BirthYear),data=sheep,family=binomial)
    summary(mod19.2) #BolCirc least significant, but keep for interaction
    #remove Horn
    mod19.3<- glmer(success~Weight+HornLen+HornCirc+BolLen+BolCirc+SibCount+VillTotal+ratio+
                    BolCirc*VillTotal+(1|BirthYear),data=sheep,family=binomial)
    summary(mod19.3) #remove interaction
    #so this becomes the same as mod11 again
    
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
    
    #re-do mod11 but include (1|MumID)
    mod18<- glmer(success~Weight+Horn+HornLen+HornCirc+Hindleg+BolLen+BolCirc+SibCount+VillTotal+
                    ratio+(1|BirthYear)+(1|MumID),data=sheep,family=binomial)
    summary(mod18) #nothing significant
    #remove Hindleg
    mod18.2<- glmer(success~Weight+Horn+HornLen+HornCirc+BolLen+BolCirc+SibCount+VillTotal+
                      ratio+(1|BirthYear)+(1|MumID),data=sheep,family=binomial)
    summary(mod18.2) #remove Horn
    mod18.3<- glmer(success~HornLen+HornCirc+BolLen+BolCirc+SibCount+VillTotal+ratio+
                      (1|BirthYear)+(1|MumID),data=sheep,family=binomial)
    summary(mod18.3) #remove BolLen
    mod18.4<- glmer(success~HornLen+HornCirc+BolCirc+SibCount+VillTotal+
                      (1|BirthYear)+(1|MumID),data=sheep,family=binomial)
    summary(mod18.4) #remove HornLen
    mod18.5<- glmer(success~HornCirc+BolCirc+SibCount+VillTotal+ratio+
                      (1|BirthYear)+(1|MumID),data=sheep,family=binomial)
    summary(mod18.5) #remove ratio
    mod18.6<- glmer(success~HornCirc+BolCirc+SibCount+VillTotal+
                      (1|BirthYear)+(1|MumID),data=sheep,family=binomial)
    summary(mod18.6) #remove HornCirc
    mod18.7<- glmer(success~BolCirc+SibCount+VillTotal+
                      (1|BirthYear)+(1|MumID),data=sheep,family=binomial)
    summary(mod18.7) #remove SibCount
    mod18.8<- glmer(success~BolCirc+VillTotal+(1|BirthYear)+(1|MumID),data=sheep,family=binomial)
    summary(mod18.8)
    #remove (1|BirthYear)
    mod18.9<- glmer(success~BolCirc+VillTotal+(1|MumID),data=sheep,family=binomial)
    summary(mod18.9)
  
  
  
### 5. Basic correlations (Future survival and reproduction) ###
  plot(DeathYear~success,data=sheep)    
  abline(lm(sheep$DeathYear~sheep$success),col="red",lwd=3)   
  cor.test(sheep$YearProp,sheep$VillTotal, method="pearson")
  
  plot(DeathYear~CountOfFirstRutOffspring,data=sheep)    
  abline(lm(sheep$DeathYear~sheep$CountOfFirstRutOffspring),col="red",lwd=3)   
  cor.test(sheep$DeathYear,sheep$CountOfFirstRutOffspring,method="pearson")
  
  plot(DeathYear~LifetimeOffspring,data=sheep)
  abline(lm(sheep$DeathYear~sheep$LifetimeOffspring),col="red",lwd=3)

  #-------------------------------------------------------------------------------  
  
  #create column for age at death (Matt's code)
  sheep$DeathAge <- with(sheep, DeathYear - BirthYear)
  #create column for SurvivedFirstYear (Matt's code)
  sheep$SurvivedFirstYear <- ifelse(sheep$DeathAge ==1,0, 1)
  
  #create column for age at death (My code)
  sheep<- sheep %>% 
    mutate(DeathAge = DeathYear-BirthYear)
  #create column for SurvivedFirstYear (My code)
  sheep<- sheep %>%
    mutate(SurvivedFirstYear = case_when(DeathAge == 1 ~ 0,   #gives 0 to all individuals that died in their first year
                                         DeathAge >= 2 ~ 1))  #gives 1 to all individuals that survived their first year
  str(sheep$SurvivedFirstYear)
  
  #--------------------------------------------------------------------------------------
  
  plot(SurvivedFirstYear~success,data=sheep)
  abline(lm(sheep$SurvivedFirstYear~sheep$success),col="red",lwd=3)
  cor.test(sheep$SurvivedFirstYear,sheep$success,method="pearson")
  #success positively correlated with first year survival
  #those that were successful in first year, survived
  
  plot(SurvivedFirstYear~CountOfFirstRutOffspring,data=sheep)
  abline(lm(sheep$SurvivedFirstYear~sheep$CountOfFirstRutOffspring),col="red",lwd=3)
  cor.test(sheep$SurvivedFirstYear,sheep$CountOfFirstRutOffspring,method="pearson")
  #increased survival with more offspring
  
  plot(SurvivedFirstYear~Weight,data=sheep)
  abline(lm(sheep$SurvivedFirstYear~sheep$Weight),col="red",lwd=3)
  cor.test(sheep$SurvivedFirstYear,sheep$Weight,method="pearson")
  #increased survival if heavier
  
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
  
  
### 6. Modelling future survival and reproduction ###
  #model Death Age 
   hist(sheep$DeathAge,breaks=50) #poisson distributed
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
   mod15.8<- glm(DeathAge~LifetimeOffspring,data=sheep,family=poisson)
   summary(mod15.8)
   
   #new version of mod15, with horn and ConFirstYear included (MERGED df)
   mod23<- glmer(DeathAge~LifetimeOffspring+CountOfFirstRutOffspring+
                   Weight+Horn+ConFirstYear+(1|BirthYear)+
                   (1|DeathYear),data=merged,family=poisson)
   summary(mod23)  #remove Weight
   mod23.2<- glmer(DeathAge~LifetimeOffspring+CountOfFirstRutOffspring+
                     Horn+ConFirstYear+(1|BirthYear)+
                     (1|DeathYear),data=merged,family=poisson)
   summary(mod23.2) #remove horn
   mod23.3<- glmer(DeathAge~LifetimeOffspring+CountOfFirstRutOffspring+
                     ConFirstYear+(1|BirthYear)+(1|DeathYear),
                     data=merged,family=poisson)
   summary(mod23.3) #remove CountOfFirstRutOffspring
   mod23.4<- glmer(DeathAge~LifetimeOffspring+ConFirstYear+
                     (1|BirthYear)+(1|DeathYear),data=merged,family=poisson)
   summary(mod23.4) #try removing random effect
   mod23.5<- glm(DeathAge~LifetimeOffspring+CountOfFirstRutOffspring+
                   Horn+ConFirstYear,
                   data=merged,family=poisson)
   summary(mod23.5) #if no random effects, lifetime offspring, first year offspring, horn and con all affect survival
   #if only (1|BirthYear)
   mod23.6<- glmer(DeathAge~LifetimeOffspring+CountOfFirstRutOffspring+
                             Horn+ConFirstYear+Weight+(1|BirthYear),
                           data=merged,family=poisson)
   summary(mod23.6) #remove weight
   mod23.7<- glmer(DeathAge~LifetimeOffspring+CountOfFirstRutOffspring+
                     Horn+ConFirstYear+(1|BirthYear),dat=merged,family=poisson)
   summary(mod23.7)
   
  #model LBS
   hist(sheep$LifetimeOffspring) #poisson (or zero inflated?)
   #try adjusting the bins
   hist(sheep$LifetimeOffspring, breaks = 10) #original hist
   hist(sheep$LifetimeOffspring, breaks = 20) #splits values
   hist(sheep$LifetimeOffspring, breaks = 60) #looks zero inflated?
   #looking at the data there are lots of zeros and 1s, two individuals over 100
   mod16<- glmer(LifetimeOffspring~CountOfFirstRutOffspring+success+Weight+BolCirc+VillTotal+
                   (1|BirthYear)+(1|DeathYear),data=sheep,family=poisson)
   summary(mod16) 
   #check for zero inflation
   check_zeroinflation(mod16) #probable zero inflation 
   #remove Weight
   mod16.2<- glmer(LifetimeOffspring~CountOfFirstRutOffspring+success+BolCirc+VillTotal+
                    (1|BirthYear)+(1|DeathYear),data=sheep,family=poisson)
   summary(mod16.2) #remove success
   mod16.3<- glmer(LifetimeOffspring~CountOfFirstRutOffspring+BolCirc+VillTotal+
                    (1|BirthYear)+(1|DeathYear),data=sheep,family=poisson)
   summary(mod16.3) #try removing (1|BirthYear)
   mod16.4<- glmer(LifetimeOffspring~CountOfFirstRutOffspring+BolCirc+VillTotal+
                     (1|DeathYear),data=sheep,family=poisson)
   summary(mod16.4)  #VillTotal becomes significant when (1|BirthYear) is removed
   #Weight is significant if BolCirc not included but BolCirc accounts for more than Weight
   
   #try same model using success rather than count 
   mod17<- glmer(LifetimeOffspring~success+BolCirc+Weight+VillTotal+
                   (1|BirthYear)+(1|DeathYear),data=sheep,family=poisson)
   summary(mod17) #remove weight
   mod17.2<- glmer(LifetimeOffspring~success+BolCirc+VillTotal+
                   (1|BirthYear)+(1|DeathYear),data=sheep,family=poisson)
   summary(mod17.2) 
   #try removing BolCirc instead
   mod17.3<- glmer(LifetimeOffspring~success+Weight+VillTotal+
                    (1|BirthYear)+(1|DeathYear),data=sheep,family=poisson)
   summary(mod17.3)
   #remove (1|BirthYear)
   mod17.4<- glmer(LifetimeOffspring~success+BolCirc+VillTotal+
                     (1|DeathYear),data=sheep,family=poisson)
   summary(mod17.4)
   mod17.5<- glm(LifetimeOffspring~success+Weight+BolCirc+VillTotal,
                   data=sheep,family=poisson)
   summary(mod17.5) #all significant
   
   #create column for subsequent offspring (not inc first year)
   sheep<- sheep %>% 
     mutate(SubsOffspring = LifetimeOffspring-CountOfFirstRutOffspring)
   
   #re-do mod17 with subsequent offspring 
   hist(sheep$SubsOffspring,breaks=70)
   mod22<- glmer(SubsOffspring~success+BolCirc+Weight+VillTotal+
                   Horn+SibCount+
                   (1|DeathYear),data=sheep,family=poisson)
   summary(mod22)
   mod22.2<- glmer(SubsOffspring~BolCirc+Weight+VillTotal+Horn+SibCount+
                     (1|DeathYear),data=sheep,family=poisson)
   summary(mod22.2)
   mod22.3<- glm(SubsOffspring~BolCirc+Weight+VillTotal+Horn,data=sheep,family=poisson)
   summary(mod22.3)  #minimal model
   
  #modelling survival of first year
   mod21<- glm(SurvivedFirstYear~success+Weight+BolCirc+VillTotal+SibCount+
                   Weight*VillTotal,data=sheep,family=binomial)
   summary(mod21) #remove SibCount
   mod21.2<- glm(SurvivedFirstYear~success+Weight+BolCirc+VillTotal+
                   Weight*VillTotal,data=sheep,family=binomial)
   summary(mod21.2) #weight least sig but keep for interaction, remove BolCirc
   mod21.3<- glm(SurvivedFirstYear~success+Weight+VillTotal+
                   Weight*VillTotal,data=sheep,family=binomial)
   summary(mod21.3) #try removing interaction
   mod21.4<- glm(SurvivedFirstYear~success+Weight+VillTotal,data=sheep,family=binomial)
   summary(mod21.4)
   #success increases survival
   #weight decreases survival
   #VillTotal decreases survival
   
   #same model but no interaction (and include ConFirstYear)
   mod24<- glm(SurvivedFirstYear~Weight+BolCirc+VillTotal+SibCount+ConFirstYear,
               data=merged,family=binomial)
   summary(mod24)  #remove ConFirstYear
   mod24.2<- glm(SurvivedFirstYear~Weight+BolCirc+VillTotal+SibCount,
                 data=merged,family=binomial)
   summary(mod24.2) #remove SibCount
   mod24.3<- glm(SurvivedFirstYear~Weight+BolCirc+VillTotal,
                 data=merged,family=binomial)
   summary(mod24.3) #remove BolCirc
   mod24.4<- glm(SurvivedFirstYear~Weight+VillTotal,data=merged,family=binomial)
   summary(mod24.4)
   
   #mod20 same as 19 but count data
   mod20<- glm(SurvivedFirstYear~CountOfFirstRutOffspring+Weight+BolCirc+VillTotal+SibCount+
                   Weight*VillTotal,data=sheep,family=binomial)
   summary(mod20) #remove SibCount
   mod20.2<- glmer(SurvivedFirstYear~CountOfFirstRutOffspring+Weight+BolCirc+VillTotal+
                     Weight*VillTotal,data=sheep,family=binomial)
   summary(mod20.2) #remove BolCirc
   mod20.3<- glm(SurvivedFirstYear~CountOfFirstRutOffspring+Weight+VillTotal+
                   Weight*VillTotal,data=sheep,family=binomial)
   summary(mod20.3)
 
# ------------------  7. PLOTTING  -------------------------
  
   #plot model 9.4
     mod9.4<- glmer(success~scale(VillTotal)+(1|BirthYear),data=sheep,family=binomial)
     summary(mod9.4)
     #plot in base R
     plot(success~VillTotal,data=sheep)
     abline(lm(sheep$success~sheep$VillTotal),col="red",lwd=3)
     cor.test(sheep$success,sheep$VillTotal,method="pearson")
     #plot using ggplot2
     plot_mod9<- ggplot(sheep,aes(VillTotal,success))+             #creates base plot
         geom_point(aes(),col="#66C2A5",size=2)+                         #adds data points, diff colours for success/not
         labs(x="Village Bay Population",y="First Year \nBreeding Success")+     #adds labels to X and Y axes
         theme_classic(base_size=10)+    #changes background colour
         stat_smooth(method="glm",method.args=list(family="binomial"),col="#FC8D62",se=FALSE)
         plot_mod9  #view plot
     
  
   #plotting mod10.5
     mod10.5<- glm(success~SibCount+VillTotal,data=sheep,family=binomial)
     summary(mod10.5)  
     #plot in base R
     plot(success~VillTotal,data=sheep,col=as.factor(SibCount)) #red twins, black single
    
     #make column with twin/singleton
     sheep<- sheep %>%
       mutate(TwinStatus=case_when(SibCount == 1 ~ "Twin",   
                                   SibCount == 0 ~ "Singleton"))
     
     #plot using ggplot2
     plot_mod10<- ggplot(sheep,aes(VillTotal,success))+                         #creates base plot
       geom_point(aes(col=TwinStatus),size=1)+                                         #adds points based on twin status
       scale_color_manual(values=c("Singleton"="#55c667ff","Twin"="#39568cff"))+    #changes colour of points
       labs(x="Village Bay Population",y="First Year Breeding \nSuccess")+  #adds labels to X and Y axes
       labs(color="Twin Status")+theme_classic(base_size=10)+ 
       stat_smooth(method="glm",method.args=list(family="binomial"),col="#440154FF",se=FALSE)
     plot_mod10      #view plot
   
   #plot mod11
     mod11.9<- glm(success~VillTotal+BolCirc,data=sheep,family=binomial)
     summary(mod11.9)
     #plot VillTotal
     p1<- ggplot(sheep,aes(VillTotal,success))+
       geom_point(aes(),col="#3cbb75ff",size=1)+
       stat_smooth(method="glm",method.args=list(family="binomial"),col="#440154FF",se=FALSE)+
       labs(x="Village Bay Population",y="First Year Breeding Success")+
       theme_classic(base_size=10)
     p1 
     #plot BolCirc
     p2<- ggplot(sheep,aes(BolCirc,success))+
       geom_point(col="#3cbb75ff",size=1)+
       stat_smooth(method="glm",method.args=list(family="binomial"),col="#440154FF",se=FALSE)+
       labs(x="Testes Circumference (mm)",y="")+
       theme_classic(base_size=10)
     p2
     #plot side by side
     plot_mod11<- p1 + p2
     plot_mod11
   
   #plot mod15
     mod15.8<- glm(DeathAge~LifetimeOffspring,data=sheep,family=poisson)
     summary(mod15.8)
     #plot
     plot_mod15<- ggplot(sheep,aes(LifetimeOffspring,DeathAge))+
       geom_point(col="#3cbb75ff",size=1)+
       geom_smooth(method=glm,se=FALSE,col="#440154FF")+
       labs(x="Lifetime Offspring",y="Age of Death")+
       theme_classic(base_size=10)
     plot_mod15
   
   #plot mod17
     mod17.5<- glm(LifetimeOffspring~success+Weight+BolCirc+VillTotal,
                   data=sheep,family=poisson)
     summary(mod17.5)
     #plot success
     plot3<- ggplot(sheep,aes(success,LifetimeOffspring))+
       geom_point()+geom_smooth(method=lm,se=FALSE,col="slateblue4")+
       theme_classic(base_size=18)
     plot3  #this seems wrong
     #plot Weight
     plot4<- ggplot(sheep,aes(Weight,LifetimeOffspring))+
       geom_point(col="slateblue4")+geom_smooth(method=NULL,se=FALSE,col="steelblue1")+
       theme_classic(base_size=18)
     plot4
     #plot BolCirc
     plot5<- ggplot(sheep,aes(BolCirc,LifetimeOffspring))+
       geom_point(col="slateblue4")+geom_smooth(method=NULL,se=FALSE,col="steelblue1")+
       theme_classic(base_size=18)
     plot5
     #plot VillTotal
     plot6<- ggplot(sheep,aes(VillTotal,LifetimeOffspring))+
       geom_point(col="slateblue4")+geom_smooth(method=lm,se=FALSE,col="steelblue1")+
       theme_classic(base_size=18)
     plot6
     #put together into panel
     plot_mod17<- (plot3+plot4)/(plot5+plot6)
     plot_mod17
 
  #plot mod21
    mod21.3<- glm(SurvivedFirstYear~success+Weight+VillTotal+
                 Weight*VillTotal,data=sheep,family=binomial)
    summary(mod21.3)
    #using geom_contour
    ggplot(sheep,aes(x=VillTotal,y=Weight,z=SurvivedFirstYear))+
      geom_density2d_filled(bins=20)+scale_fill_viridis(option="inferno",discrete=TRUE)+
      labs(x="Village Bay Population",y="August Weight \n(kg)",
      fill="First Year Survival")+theme_classic(base_size=10)
    
    #transform pop into categorical "high" or "low"
    median(sheep$VillTotal,na.rm=FALSE) #median pop size is 494
    
    sheep<- sheep %>%
      mutate(PopType=case_when(VillTotal <494 ~ 0,   
                               VillTotal >=494 ~ 1))
    
    
###---------------------------------------------------------------------------------###
   
     #re-run mod21 with population as categorical
    mod21.3<- glm(SurvivedFirstYear~success+Weight+VillTotal+
                    Weight*VillTotal,data=sheep,family=binomial)
    summary(mod21.3)
    
    mod21.5<- glm(SurvivedFirstYear~success+Weight+PopType+
                    Weight*PopType,data=sheep,family=binomial)
    summary(mod21.5)
    
    #using code from hawthorn project to plot
    table(sheep$PopType) #shows how many cases there are of high (1) and low (0) pop
    
    #create table of coefficients to use when plotting
    cf<- summary(mod21.5)$coefficients
    cf
    
    #plot in base R
    plot(Weight~SurvivedFirstYear,data=sheep,col=as.factor(PopType))
    
    
    
    
    
###--------------------------------------------------------------------------------###
    
    #plot for success
    plot8<- ggplot(sheep,aes(success,SurvivedFirstYear))+
      geom_bar(stat="identity",width=0.5)+
      labs(x="First year breeding success",y="First year survival")+
      theme_classic(base_size=10)
    plot8
    
    #combine plots
    plot_mod21<- plot7+plot8
    plot_mod21
    
    
    #plot_mod22
    mod22.3<- glmer(SubsOffspring~BolCirc+Weight+VillTotal+Horn,
                    data=sheep,family=poisson)
    summary(mod22.3)
    #plot BolCirc
    plot9<- ggplot(sheep,aes(BolCirc,SubsOffspring))+            
      geom_point(aes(),col="#D14E72FF",size=1,alpha=0.7)+                         
      labs(x="Testes circumference (mm)",y="Subsequent offspring")+     
      theme_classic(base_size=10)+    
      stat_smooth(method="glm",method.args=list(family="poisson"),
                  col="#8305A7FF",se=FALSE) 
    plot9
    #plot Weight
    plot10<- ggplot(sheep,aes(Weight,SubsOffspring))+            
      geom_point(aes(),col="#D14E72FF",size=1,alpha=0.7)+                         
      labs(x="August weight (kg)",y="Subsequent offspring")+     
      theme_classic(base_size=10)+    
      stat_smooth(method="glm",method.args=list(family="poisson"),
                  col="#8305A7FF",se=FALSE) 
    plot10
    #plotVillTotal
    plot11<- ggplot(sheep,aes(VillTotal,SubsOffspring))+            
      geom_point(aes(),col="#D14E72FF",size=1,alpha=0.7)+                         
      labs(x="Village Bay population",y="Subsequent offspring")+     
      theme_classic(base_size=10)+    
      stat_smooth(method="glm",method.args=list(family="poisson"),
                  col="#8305A7FF",se=FALSE) 
    plot11
    
    #make categorical column for horn type
    sheep<- sheep %>%
      mutate(HornType=case_when(Horn == 3 ~ "Normal",   
                                Horn == 1 ~ "Scurred"))
    #plot Horn (bar chart)
    plot12<- ggplot(data=sheep,aes(HornType,SubsOffspring,fill=HornType)) +
      geom_bar(stat="identity",width=0.5)+theme_classic(base_size=10)+
      labs(x="Horn Type",y="Subsequent Offspring")+
      scale_fill_manual(name="Horn Type",values=c("#D14E72FF","#5901A5FF","#FEB72DFF"))
    plot12
    
    #finding average and SE for each horn type
    sheepNormal<- sheep %>%
      filter(Horn==3)
    View(sheepNormal)
    meanNormalSubs<- mean(sheepNormal$SubsOffspring)
    meanNormalSubs
    
    SEnormal<- std.error(sheepNormal$SubsOffspring)
    SEnormal
    
    sheepScurred<- sheep %>%
      filter(Horn==1)
    View(sheepScurred)
    meanScurredSubs<- mean(sheepScurred$SubsOffspring)
    meanScurredSubs
    
    SEscurred<- std.error(sheepScurred$SubsOffspring)
    SEscurred
    
    HornMeans <- read_excel("~/University/4th Year/Dissertation/HornMeans.xlsx")
    View(HornMeans)
    
    plot14<- ggplot(HornMeans,aes(HornType,MeanSubsOff,fill=HornType))+
      geom_bar(stat="identity",width=0.5)+theme_classic(base_size=10)+
      labs(x="Horn Type",y="Subsequent Offspring")+
      scale_fill_manual(name="Horn Type",values=c("#D14E72FF","#8305A7FF"))+
      geom_errorbar(aes(ymin=MeanSubsOff-SE,ymax=MeanSubsOff+SE),width=.2,
                    lwd=1,position=position_dodge(.9))
    plot14
    
    #combine plots
    plot_mod22<- (plot9+plot10)/(plot11+plot14)
    plot_mod22
    
    #plot_mod23
    mod23.5<- glm(DeathAge~LifetimeOffspring+CountOfFirstRutOffspring+
                    Horn+ConFirstYear,data=merged,family=poisson)
    summary(mod23.5)
    #plot Lifetime Offspring
    plot15<- ggplot(merged,aes(LifetimeOffspring,DeathAge))+            
      geom_point(aes(),col="#8DD3C7",size=1,alpha=0.7)+                         
      labs(x="Lifetime Breeding Success",y="Age of Death")+     
      theme_classic(base_size=10)+    
      stat_smooth(method="glm",method.args=list(family="poisson"),
                  col="#BC80BD",se=FALSE) 
    plot15
    
    #plot first rut offspring
    plot16<- ggplot(merged,aes(CountOfFirstRutOffspring,DeathAge))+            
      geom_point(aes(),col="#8DD3C7",size=1,alpha=0.7)+                         
      labs(x="Offspring in first year",y="Age of Death")+     
      theme_classic(base_size=10)+    
      stat_smooth(method="glm",method.args=list(family="poisson"),
                  col="#BC80BD",se=FALSE) 
    plot16
    
    #plot Con First Year
    plot17<- ggplot(merged,aes(x=ConFirstYear,y=DeathAge))+            
      geom_point(aes(),col="#8DD3C7",size=1,alpha=0.7)+                         
      labs(x="Consort in first year?",y="Age of Death")+     
      theme_classic(base_size=10)+    
      stat_smooth(method="glm",method.args=list(family="poisson"),
                  col="#BC80BD",se=FALSE) 
    plot17

    #make categorical column for ConFirstYear
    merged<- merged %>%
      mutate(Consorted=case_when(ConFirstYear == 1 ~ "Yes",   
                                ConFirstYear == 0 ~ "No"))
    str(merged$ConFirstYear)
    summary(merged$ConFirstYear)
    
    plot17.2<- ggplot(data=subset(merged, !is.na(Consorted)), 
      aes(x=Consorted,y=DeathAge,fill=Consorted))+
      geom_boxplot(color="#737373")+
      labs(x="Held consort in first year?",y="Age of death")+
      theme_classic(base_size=10)+
      scale_fill_manual(values=c("#8DD3C7","#BC80BD"))
    plot17.2
    
    #plot Horn
    plot18<- ggplot(merged,aes(Horn,DeathAge))+            
      geom_point(aes(),col="#8DD3C7",size=1,alpha=0.7)+                         
      labs(x="Horn Type",y="Age of Death")+     
      theme_classic(base_size=10)+    
      stat_smooth(method="glm",method.args=list(family="poisson"),
                  col="#BC80BD",se=FALSE)
    plot18  #this should be a bar chart
    
    #finding average and SE for each horn type (death age)
    sheepNormal<- sheep %>%
      filter(Horn==3)
    View(sheepNormal)
    meanNormalDeath<- mean(sheepNormal$DeathAge)
    meanNormalDeath
    
    SEnormalDeath<- std.error(sheepNormal$DeathAge)
    SEnormalDeath
    
    sheepScurred<- sheep %>%
      filter(Horn==1)
    View(sheepScurred)
    meanScurredDeath<- mean(sheepScurred$DeathAge)
    meanScurredDeath
    
    SEscurredDeath<- std.error(sheepScurred$DeathAge)
    SEscurredDeath
    
    HornMeans <- read_excel("~/University/4th Year/Dissertation/HornMeans.xlsx")
    View(HornMeans)
    
    plot19<- ggplot(HornMeans,aes(HornType,MeanDeath,fill=HornType))+
      geom_bar(stat="identity",width=0.5,color="#737373")+theme_classic(base_size=10)+
      labs(x="Horn Type",y="Age of death")+
      scale_fill_manual(name="Horn Type",values=c("#8DD3C7","#BC80BD"))+
      geom_errorbar(aes(ymin=MeanDeath-SEdeath,ymax=MeanDeath+SEdeath),width=.2,
                    lwd=1,position=position_dodge(.9))
    plot19
    
    
    plot_mod23<- (plot15+plot16)/(plot17.2+plot19)
    plot_mod23
    
    #plot mod24
    mod24.4<- glm(SurvivedFirstYear~Weight+VillTotal,data=merged,family=binomial)
    summary(mod24.4)
    
    #plot for Weight
    plot20<- ggplot(merged,aes(Weight,SurvivedFirstYear))+         
      geom_point(aes(),col="#8DD3C7",size=1,alpha=0.1)+                         
      labs(x="Weight",y="Survived first year")+     
      theme_classic(base_size=10)+    
      stat_smooth(method="glm",method.args=list(family="binomial"),
                  col="#BC80BD",se=FALSE)
    plot20
    
    #plot for VillTotal
    plot21<- ggplot(merged,aes(VillTotal,SurvivedFirstYear))+         
      geom_point(aes(),col="#8DD3C7",size=1,alpha=1)+                         
      labs(x="Village bay population",y="Survived first year")+     
      theme_classic(base_size=10)+    
      stat_smooth(method="glm",method.args=list(family="binomial"),
                  col="#BC80BD",se=FALSE)
    plot21
    
    plot_mod24<- plot20+plot21
    plot_mod24
    
    show_col(viridis_pal(option="plasma")(20))
    show_col(viridis_pal(option="magma")(20))
    show_col(viridis_pal(option="inferno")(20))
    show_col(viridis_pal()(20))
    display.brewer.pal(n = 8, name = 'Set2')
    brewer.pal(n = 8, name = "Set2")
    display.brewer.pal(n=10,name="Set3")
    brewer.pal(n=10,name="Set3")
    display.brewer.pal(n=8,name="Greys")
    brewer.pal(n=8,name="Greys")
    
    
    plot_mod9
    plot_mod10
    plot_mod11
    plot_mod15
    plot_mod21
    plot_mod22
    
    
  