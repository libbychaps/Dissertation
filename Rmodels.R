# 1. What factors are associated with ram lamb success?
  # mod9: VillTotal decreases success (all sheep)
  # mod10: SibCount and VillTotal decrease success 
  # mod11: BolCirc increases success, VillTotal decreaases (Aug catch animals)

 mod9.6<- glm(success~VillTotal,data=sheep,family=binomial)
 summary(mod9.6) 
 
 mod10.5<- glm(success~SibCount+VillTotal,data=sheep,family=binomial)
 summary(mod10.5)

 mod11.12<- glm(success~BolCirc+VillTotal,data=sheep,family=binomial)
 summary(mod11.12)
 
# 2. How does first year reproduction relate to future survival and reproduction?
  # mod15: Individuals with more offspring have lived longer (vice versa)
  # mod16: More offspirng in first rut inc. LBS, BolCirc inc., VillTotal dec. (but zero inflated)
  # mod17: success, Weight, BolCirc inc. LBS, VillTotal dec. (but zero inflated)
  # mod21: success increases survival, Weight*VillTotal increases survival
 
 mod15.8<- glm(DeathAge~LifetimeOffspring,data=sheep,family=poisson)
 summary(mod15.8)
 
 mod16.4<- glmer(LifetimeOffspring~CountOfFirstRutOffspring+BolCirc+VillTotal+(1|DeathYear),data=sheep,family=poisson)
 summary(mod16.4) 
 
 mod17.5<- glm(LifetimeOffspring~success+Weight+BolCirc+VillTotal,data=sheep,family=poisson)
 summary(mod17.5)
 
 mod21.3<- glm(SurvivedFirstYear~success+Weight+VillTotal+Weight*VillTotal,data=sheep,family=binomial)
 summary(mod21.3)
 
# 3. Is holding a consort associated with the success of ram lambs?
  # modA: holding a consort does not affect success in first year
 
 modA.3<- glm(success~VillTotal+Weight+BolCirc,data=merged,family=binomial)
 summary(modA.3)
 #ConFirstYear was removed from maximal model because insignificant

# 4. What factors determine appearance in a consort?
  # modB: BolCirc increases appearance in rut, VillTotal*Weight decreases appearance in rut
 
 modB.5<- glm(ConFirstYear~VillTotal+Weight+BolCirc+Weight*VillTotal,data=merged,family=binomial)
 summary(modB.5) 
 