rm(list=ls())
consorts <- read_excel("~/University/4th Year/Dissertation/consorts.xlsx")
View(consorts)

# 1. finding out how many of the observations were first year males

  #create column for age at consort
  consorts<- consorts %>% 
    mutate(ConAge = ObsYear-TupBirthYear)

  #create binary column for consort first year consort
  consorts<- consorts %>%
    mutate(ConFirstYear = case_when(ConAge == 0 ~ "1",   #gives 1 to consorts held by ram lambs
                                    ConAge >= 1 ~ "0"))  #gives 0 to consorts held by adult rams

########this should be converted to numeric but not letting me
  
  NumConFirstYear<- length(consorts$ConFirstYear[consorts$ConFirstYear == 1]) #count first year consorts
  NumConFirstYear #2260 consorts held by first year rams (THIS IS WRONG)
  nrow(consorts)  #11273 total consort obs
  PropCon = (NumConFirstYear/11273)  #proportion of obs that were first year males 
  PropCon   #0.200479 (20%)
  
  #another way to check this
  df1<- consorts[consorts$ConFirstYear == 1, ]  #subsets all data for first year consorts
  View(df1)  
  NumConFirstYear<- length(df1$ConFirstYear[df1$ConFirstYear==1])  #counts all first year consorts 
  NumConFirstYear
  propCon = (NumConFirstYear/11273)
  propCon 
  
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
  PropCon2  #3.5% 
  
  #check this
  df2<- consorts2[consorts2$ConFirstYear == 1, ]  #subsets all data for first year consorts
  View(df2)  
  NumConFirstYear2<- length(df2$ConFirstYear[df2$ConFirstYear==1])  #counts all first year consorts 
  NumConFirstYear2
  propCon2 = (NumConFirstYear2/9346)
  propCon2 
  

  #Merging data sets  (code from Monica)
  data_with_block_means <- merge(density_data_limited, 
                                 block_density_means,
                                 by=c("year","block"))
  
  new_df <- merge(main_df, small_df, by="column with the same factors")
