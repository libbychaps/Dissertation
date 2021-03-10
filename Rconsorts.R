rm(list=ls())
consorts <- read_excel("~/University/4th Year/Dissertation/consorts.xlsx")
View(consorts)

# 1. finding out how many of the observations were first year males

  #create column for age at consort
  consorts<- consorts %>% 
    mutate(ConAge = ObsYear-TupBirthYear)

  #create binary column for consort first year consort
  consorts<- consorts %>%
    mutate(ConFirstYear = case_when(ConAge == 0 ~ 1,   #gives 1 to consorts held by ram lambs
                                    ConAge >= 1 ~ 0))  #gives 0 to consorts held by adult rams
  str(consorts$ConFirstYear)  #checkthis is formatted as num
  
  #counting number of consorts held by first year rams
  NumConFirstYear<- table(consorts$ConFirstYear)[names(table(consorts$ConFirstYear)) == 1] #count first year consorts
  NumConFirstYear #333 consorts held by first year rams 
  nrow(consorts)  #11273 total consort obs
  PropCon = (NumConFirstYear/11273)  #proportion of obs that were first year males 
  PropCon   #0.02953961 (~2.9%)
  
# 2. Create new data set, deleting entries where male age unknown
  consorts2<- data.frame(consorts) #replicated df
  View(consorts2)
  nrow(consorts2)  #gives total number of rows (11273)
  sum(is.na(consorts2$TupBirthYear)) #1927 entries with unknown TupBirthYear
  colSums(is.na(consorts2))  #tells us how many NA in each column
  consorts2<- consorts2[complete.cases(consorts2$TupBirthYear),]  #deletes cases where TupBirthYear NA
  View(consorts2)
  nrow(consorts2)  #gives number of rows with omitted data (11273-1927 = 9346)
  
  #of this new data set, what proportion of individuals are 1st year
  NumConFirstYear2<- length(consorts2$ConFirstYear[consorts2$ConFirstYear == 1]) #count obs of first year consorts
  NumConFirstYear2  #333 obs of first year consorts
  nrow(consorts2)  #9346 total consort obs
  PropCon2 = (NumConFirstYear2/9346)  #proportion of obs that were first year males 
  PropCon2  #0.03573721 (~3.6%) 

# 3. Merging data sets 
  #rename ID column so both data frames match
  consorts2 <- consorts2 %>% 
    rename(ID = TupID)          #renames TupID column to ID
  
  merged<- merge(sheep,consorts2,         #select df to merge
                 by="ID",                 #merge by common column
                 all.x=TRUE, all.y=TRUE)  #keep all rows of both data frames
  View(merged)
  
#4. Basic correlations
  #correlation between success and consort in first year
  plot(success~ConFirstYear,data=merged)
  abline(lm(merged$success~merged$ConFirstYear),col="red",lwd=3) 
  cor.test(merged$success,merged$ConFirstYear,method="pearson") 
  #no effect of consort on first year success
  
  #correlation between weight and consort in first year
  plot(ConFirstYear~Weight,data=merged)
  abline(lm(merged$ConFirstYear~merged$Weight),col="red",lwd=3)
  cor.test(merged$ConFirstYear,merged$Weight,method="pearson")  
  #appear in the rut less if they are heavier - good quality afford to wait a year?
  
  #correlation between BolCirc and consort
  plot(ConFirstYear~BolCirc,data=merged)
  abline(lm(merged$ConFirstYear~merged$BolCirc),col="red",lwd=3)  
  cor.test(merged$ConFirstYear,merged$BolCirc,method="pearson")
  #BolCirc does not influence appearance in rut
  
  #correlation between horn and consort
  plot(ConFirstYear~Horn,data=merged)
  abline(lm(merged$ConFirstYear~merged$Horn),col="red",lwd=3)  
  cor.test(merged$ConFirstYear,merged$Horn,method="pearson")  
  #horn type does not affect appearance in rut
  
  #correlation between sib count and consort
  plot(ConFirstYear~SibCount,data=merged)
  abline(lm(merged$ConFirstYear~merged$SibCount),col="red",lwd=3)
  cor.test(merged$ConFirstYear,merged$SibCount,method="pearson")  
  #sib count does not affect appearance in rut  
  
  #correlation between VillTotal and consort
  plot(ConFirstYear~VillTotal,data=merged)
  abline(lm(merged$ConFirstYear~merged$VillTotal),col="red",lwd=3)
  cor.test(merged$ConFirstYear,merged$VillTotal,method="pearson")  
  #population has no affect on appearance in rut
  
  #correlation between ratio and consort
  plot(ConFirstYear~ratio,data=merged)
  abline(lm(merged$ConFirstYear~merged$ratio),col="red",lwd=3)  
  cor.test(merged$ConFirstYear,merged$ratio,method="pearson")  
  #increase in first year consorts with inc female:male ratio  