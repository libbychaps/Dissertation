# 1. What factors are associated with ram lamb success?
  # mod9: VillTotal decreases success (all sheep)
  # mod10: SibCount and VillTotal decrease success 
  # mod11: BolCirc increases success, VillTotal decreaases (Aug catch animals)

 mod9.6<- glm(success~VillTotal,data=sheep,family=binomial)
 summary(mod9.6) 
 plot_mod9
 
 mod10.5<- glm(success~SibCount+VillTotal,data=sheep,family=binomial)
 summary(mod10.5)
 plot_mod10

 mod11.12<- glm(success~BolCirc+VillTotal,data=sheep,family=binomial)
 summary(mod11.12)
 plot_mod11
 
# 2. How does first year reproduction relate to future survival and reproduction?
 #SURVIVAL
  # mod24: Weight increases first year survival, population decreases survival
 #FUTURE REPRO
  # mod22: bolCirc, weight and horn increase subsequent offspring, villtotal decreases

 mod21.3<- glm(SurvivedFirstYear~success+Weight+VillTotal+Weight*VillTotal,data=sheep,family=binomial)
 summary(mod21.3) 
 
 mod22.4<- glm(SubsOffspring~success+BolCirc+Weight+VillTotal+Horn+Weight*VillTotal,
               data=sheep,family=poisson)
 summary(mod22.4)
 
 
# 3. Is holding a consort associated with the success of ram lambs?
  # modA: holding a consort does not affect success in first year
 
 modA.4<- glm(success~VillTotal+Weight+BolCirc,data=merged,family=binomial)
 summary(modA.4)
 #ConFirstYear was removed from maximal model because insignificant

# 4. What factors determine appearance in a consort?
  # modB: BolCirc increases appearance in rut, VillTotal*Weight decreases appearance in rut
 
 modB.5<- glm(ConFirstYear~VillTotal+Weight+BolCirc+Weight*VillTotal,data=merged,family=binomial)
 summary(modB.5) 
 plotA
 
 