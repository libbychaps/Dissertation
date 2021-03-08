consorts <- read_excel("~/University/4th Year/Dissertation/consorts.xlsx")
View(consorts)

# 1. finding out how many of the observations were first year males

  #create column for age at consort
  consorts<- consorts %>% 
    mutate(ConAge = ObsYear-TupBirthYear)

  #create binary column if consort in first year
  consorts<- consorts %>%
    mutate(ConFirstYear = case_when(ConAge == 0 ~ "1",   #gives 1 to all individuals that consorted in their first year
                                   ConAge >= 1 ~ "0"))  #gives 0 to all individuals that did not hold consort in first year

  NumConFirstYear<- length(consorts$ConFirstYear[consorts$ConFirstYear == 1]) #2260 obs of first year consorts
  length(consorts$Date)  #11273 total consort obs
  PropCon = (NoConFirstYear/11273)  #proportion of obs that were first year males --> 0.200479
  PropCon
