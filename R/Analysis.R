Sens<- read.csv('Data/Shape sen for each component_ANOVA.csv', header = TRUE)
Comps<- read.csv('Data/Components_ANOVA_format.csv', header = TRUE)
Sens$Component_Value<- as.factor(Sens$Component_Value)
Comps$Component_Value<- as.factor(Comps$Component_Value)
Sens1<- Sens[Sens$Object_Type == "Novel",]
Sens2<- Sens[Sens$Object_Type == "Semantic",]
Comps1<- Comps[Comps$Object_Type == "Novel",]
Comps2<- Comps[Comps$Object_Type == "Semantic",]

  library('ez')
library('psychReport')
fullmodel <- ezANOVA(data=Sens2[Sens2$Pathway == "Ventral",],
                       dv=Betas,
                       wid=ID,
                       within=.(Component_Value),
                       type=3,
                       return_aov=TRUE)

fullmodel <- ezANOVA(data=Sens2[Sens2$Pathway == "Dorsal",],
                     dv=Betas,
                     wid=ID,
                     within=.(Component_Value),
                     type=3,
                     return_aov=TRUE)
fullmodel

fullmodel <- ezANOVA(data=Sens2,
                     dv=Betas,
                     wid=ID,
                     within=.(Component_Value, Pathway),
                     type=3,
                     return_aov=TRUE)

aovEffectSize(fullmodel$aov, effectSize = 'pes')

fullmodel <- ezANOVA(data=Comps2[Comps2$Pathway == "Ventral",],
                     dv=Betas,
                     wid=ID,
                     within=.(Component_Value),
                     type=3,
                     return_aov=TRUE)

fullmodel <- ezANOVA(data=Comps2[Comps2$Pathway == "Dorsal",],
                     dv=Betas,
                     wid=ID,
                     within=.(Component_Value),
                     type=3,
                     return_aov=TRUE)
fullmodel

fullmodel <- ezANOVA(data=Comps2,
                     dv=Betas,
                     wid=ID,
                     within=.(Component_Value, Pathway),
                     type=3,
                     return_aov=TRUE)

aovEffectSize(fullmodel$aov, effectSize = 'pes')


library(MBESS)
Upper.lim<- c()
Lower.lim<- c()
for (i in 1:15){
Lims <- conf.limits.ncf(F.value = fullmodel$ANOVA[i,4], conf.level = 0.90, df.1 <- 1, df.2 <- 11)
Lower.lim[i] <- Lims$Lower.Limit/(Lims$Lower.Limit + df.1 + df.2 + 1)
Upper.lim[i] <- Lims$Upper.Limit/(Lims$Upper.Limit + df.1 + df.2 + 1)
}
CIs<-cbind(Upper.lim, Lower.lim)

fullmodel2 <- ezANOVA(data=Comps,
                     dv=Betas,
                     wid=ID,
                     within =.(Component_Value, Pathway, Object_Type),
                     type=3,
                     return_aov=TRUE)
fullmodel2
aovEffectSize(fullmodel2$aov, effectSize = 'pes')

library(MBESS)
Upper.lim<- c()
Lower.lim<- c()
for (i in 1:15){
  Lims <- conf.limits.ncf(F.value = fullmodel2$ANOVA[i,4], conf.level = 0.90, df.1 <- 1, df.2 <- 11)
  Lower.lim[i] <- Lims$Lower.Limit/(Lims$Lower.Limit + df.1 + df.2 + 1)
  Upper.lim[i] <- Lims$Upper.Limit/(Lims$Upper.Limit + df.1 + df.2 + 1)
}
CIs<-cbind(Upper.lim, Lower.lim)

intercorrs <- read.table("Data/intersubjectcorrelation_ANOVA.xlsx")


intercorrs$Subject<- as.factor(intercorrs$Subject)
intercorrs$Object<- as.factor(intercorrs$Object)
intercorrs$Stream<- as.factor(intercorrs$Stream)
intercorrs$Hemisphere<- as.factor(intercorrs$Hemisphere)
intercorrs$Part<- as.factor(intercorrs$Part)
fullmodel1 <- ezANOVA(data=intercorrs,
                     dv=Correlation,
                     wid=Subject,
                     within = c( Object, Stream, Part),
                     type=3,
                     return_aov=TRUE)
fullmodel1

library(MBESS)
Upper.lim<- c()
Lower.lim<- c()
for (i in 1:15){
  Lims <- conf.limits.ncf(F.value = fullmodel1$ANOVA[i,4], conf.level = 0.90, df.1 <- 1, df.2 <- 11)
  Lower.lim[i] <- Lims$Lower.Limit/(Lims$Lower.Limit + df.1 + df.2 + 1)
  Upper.lim[i] <- Lims$Upper.Limit/(Lims$Upper.Limit + df.1 + df.2 + 1)
}
CIs<-cbind(Upper.lim, Lower.lim)

aovEffectSize(fullmodel1$aov, effectSize = 'pes')





##Planned Comparisons follow-up for significant ANOVA

#first looking at only semantic data
#pull out only dorsal pathway then change that to say ventral and look at the two levels of components
mydata<-Sens2[Sens2$Pathway == "Dorsal",]
c2<-rep(rep(c(-1, 1), each = 12), times = 2)
c1<-rep(rep(c(1, -1), each = 12), times = 2)
mydata$c2<- c2
mydata$c1<- c1
anova(lm(Betas ~ c1 + c2, mydata))

#pull out only 1st component then change that to say 2 and look at the two pathways
mydata<-Sens2[Sens2$Component_Value == 1,]
c2<-rep(c(-1, 1), each = 24)
c1<-rep(c(1, -1), each = 24)
mydata$c2<- c2
mydata$c1<- c1
anova(lm(Betas ~ c1 + c2, mydata))

#pull out only dorsal pathway then change that to say ventral and look at the two levels of components
mydata<-Comps2[Comps2$Pathway == "Ventral",]
c2<-rep(rep(c(-1, 1), each = 12), times = 2)
c1<-rep(rep(c(1, -1), each = 12), times = 2)
mydata$c2<- c2
mydata$c1<- c1
anova(lm(Betas ~ c1 + c2, mydata))

#pull out only 1st component then change that to say 2 and look at the two pathways
mydata<-Comps2[Comps2$Component_Value == 2,]
c2<-rep(c(-1, 1), each = 24)
c1<-rep(c(1, -1), each = 24)
mydata$c2<- c2
mydata$c1<- c1
anova(lm(Betas ~ c1 + c2, mydata))





#Second looking at only Novel data
#pull out only dorsal pathway then change that to say ventral and look at the two levels of components
mydata<-Sens1[Sens1$Pathway == "Ventral",]
c2<-rep(rep(c(-1, 1), each = 12), times = 2)
c1<-rep(rep(c(1, -1), each = 12), times = 2)
mydata$c2<- c2
mydata$c1<- c1
anova(lm(Betas ~ c1 + c2, mydata))

#pull out only 1st component then change that to say 2 and look at the two pathways
mydata<-Sens1[Sens1$Component_Value == 2,]
c2<-rep(c(-1, 1), each = 24)
c1<-rep(c(1, -1), each = 24)
mydata$c2<- c2
mydata$c1<- c1
anova(lm(Betas ~ c1 + c2, mydata))

#pull out only dorsal pathway then change that to say ventral and look at the two levels of components
mydata<-Comps1[Comps1$Pathway == "Dorsal",]
c2<-rep(rep(c(-1, 1), each = 12), times = 2)
c1<-rep(rep(c(1, -1), each = 12), times = 2)
mydata$c2<- c2
mydata$c1<- c1
anova(lm(Betas ~ c1 + c2, mydata))

#pull out only 1st component then change that to say 2 and look at the two pathways
mydata<-Comps1[Comps1$Component_Value == 2,]
c2<-rep(c(-1, 1), each = 24)
c1<-rep(c(1, -1), each = 24)
mydata$c2<- c2
mydata$c1<- c1
anova(lm(Betas ~ c1 + c2, mydata))




##I want to do planned comparisons for the bigger comparison between novel and semantic

#pull out only dorsal pathway  


mydata<-Sens[Sens$Pathway == "Dorsal",]
c2<-rep(rep(c(-1, 1), each = 48), times = 1)
c1<-rep(rep(c(1, -1), each = 48), times = 1)
mydata$c2<- c2
mydata$c1<- c1
anova(lm(Betas ~ c1 + c2, mydata))



mydata<-Sens[Sens$Pathway == "Ventral",]
c2<-rep(rep(c(-1, 1), each = 48), times = 1)
c1<-rep(rep(c(1, -1), each = 48), times = 1)
mydata$c2<- c2
mydata$c1<- c1
anova(lm(Betas ~ c1 + c2, mydata))


mydata<-Sens[Sens$Pathway == "Dorsal" & Sens$Component_Value == "1",]
c2<-rep(rep(c(-1, 1), each = 24), times = 1)
c1<-rep(rep(c(1, -1), each = 24), times = 1)
mydata$c2<- c2
mydata$c1<- c1
anova(lm(Betas ~ c1 + c2, mydata))

mydata<-Sens[Sens$Pathway == "Dorsal" & Sens$Component_Value == "2",]
c2<-rep(rep(c(-1, 1), each = 24), times = 1)
c1<-rep(rep(c(1, -1), each = 24), times = 1)
mydata$c2<- c2
mydata$c1<- c1
anova(lm(Betas ~ c1 + c2, mydata))

mydata<-Sens[Sens$Pathway == "Ventral" & Sens$Component_Value == "1",]
c2<-rep(rep(c(-1, 1), each = 24), times = 1)
c1<-rep(rep(c(1, -1), each = 24), times = 1)
mydata$c2<- c2
mydata$c1<- c1
anova(lm(Betas ~ c1 + c2, mydata))

mydata<-Sens[Sens$Pathway == "Ventral" & Sens$Component_Value == "2",]
c2<-rep(rep(c(-1, 1), each = 24), times = 1)
c1<-rep(rep(c(1, -1), each = 24), times = 1)
mydata$c2<- c2
mydata$c1<- c1
anova(lm(Betas ~ c1 + c2, mydata))




##now look at the component correlation values. 

mydata<-Comps[Comps$Pathway == "Dorsal",]
c2<-rep(rep(c(-1, 1), each = 48), times = 1)
c1<-rep(rep(c(1, -1), each = 48), times = 1)
mydata$c2<- c2
mydata$c1<- c1
anova(lm(Betas ~ c1 + c2, mydata))



mydata<-Comps[Comps$Pathway == "Ventral",]
c2<-rep(rep(c(-1, 1), each = 48), times = 1)
c1<-rep(rep(c(1, -1), each = 48), times = 1)
mydata$c2<- c2
mydata$c1<- c1
anova(lm(Betas ~ c1 + c2, mydata))


mydata<-Comps[Comps$Pathway == "Dorsal" & Comps$Component_Value == "1",]
c2<-rep(rep(c(-1, 1), each = 24), times = 1)
c1<-rep(rep(c(1, -1), each = 24), times = 1)
mydata$c2<- c2
mydata$c1<- c1
anova(lm(Betas ~ c1 + c2, mydata))

mydata<-Comps[Comps$Pathway == "Dorsal" & Comps$Component_Value == "2",]
c2<-rep(rep(c(-1, 1), each = 24), times = 1)
c1<-rep(rep(c(1, -1), each = 24), times = 1)
mydata$c2<- c2
mydata$c1<- c1
anova(lm(Betas ~ c1 + c2, mydata))

mydata<-Comps[Comps$Pathway == "Ventral" & Comps$Component_Value == "1",]
c2<-rep(rep(c(-1, 1), each = 24), times = 1)
c1<-rep(rep(c(1, -1), each = 24), times = 1)
mydata$c2<- c2
mydata$c1<- c1
anova(lm(Betas ~ c1 + c2, mydata))

mydata<-Comps[Comps$Pathway == "Ventral" & Comps$Component_Value == "2",]
c2<-rep(rep(c(-1, 1), each = 24), times = 1)
c1<-rep(rep(c(1, -1), each = 24), times = 1)
mydata$c2<- c2
mydata$c1<- c1
anova(lm(Betas ~ c1 + c2, mydata))



##Now i want to look at the differences in correlations.


mydata<-intercorrs[intercorrs$Object == "Semantic" & intercorrs$Stream == "Dorsal",]
c2<-rep(rep(c(-1, 1), each = 12), times = 2)
c1<-rep(rep(c(1, -1), each = 12), times = 2)
mydata$c2<- c2
mydata$c1<- c1
anova(lm(Correlation ~ c1 + c2, mydata))

mydata<-intercorrs[intercorrs$Object == "Semantic" & intercorrs$Stream == "Ventral",]
c2<-rep(rep(c(-1, 1), each = 12), times = 2)
c1<-rep(rep(c(1, -1), each = 12), times = 2)
mydata$c2<- c2
mydata$c1<- c1
anova(lm(Correlation ~ c1 + c2, mydata))

mydata<-intercorrs[intercorrs$Object == "Semantic" & intercorrs$Part == "Anterior",]
c2<-rep(rep(c(-1, 1), each = 24), times = 1)
c1<-rep(rep(c(1, -1), each = 24), times = 1)
mydata$c2<- c2
mydata$c1<- c1
anova(lm(Correlation ~ c1 + c2, mydata))

mydata<-intercorrs[intercorrs$Object == "Semantic" & intercorrs$Part == "Posterior",]
c2<-rep(rep(c(-1, 1), each = 24), times = 1)
c1<-rep(rep(c(1, -1), each = 24), times = 1)
mydata$c2<- c2
mydata$c1<- c1
anova(lm(Correlation ~ c1 + c2, mydata))


mydata<-intercorrs[intercorrs$Object == "Novel" & intercorrs$Stream == "Dorsal",]
c2<-rep(rep(c(-1, 1), each = 12), times = 2)
c1<-rep(rep(c(1, -1), each = 12), times = 2)
mydata$c2<- c2
mydata$c1<- c1
anova(lm(Correlation ~ c1 + c2, mydata))


mydata<-intercorrs[intercorrs$Object == "Novel" & intercorrs$Stream == "Ventral",]
c2<-rep(rep(c(-1, 1), each = 12), times = 2)
c1<-rep(rep(c(1, -1), each = 12), times = 2)
mydata$c2<- c2
mydata$c1<- c1
anova(lm(Correlation ~ c1 + c2, mydata))


mydata<-intercorrs[intercorrs$Object == "Novel" & intercorrs$Part == "Anterior",]
c2<-rep(rep(c(-1, 1), each = 24), times = 1)
c1<-rep(rep(c(1, -1), each = 24), times = 1)
mydata$c2<- c2
mydata$c1<- c1
anova(lm(Correlation ~ c1 + c2, mydata))

mydata<-intercorrs[intercorrs$Object == "Novel" & intercorrs$Part == "Posterior",]
c2<-rep(rep(c(-1, 1), each = 24), times = 1)
c1<-rep(rep(c(1, -1), each = 24), times = 1)
mydata$c2<- c2
mydata$c1<- c1
anova(lm(Correlation ~ c1 + c2, mydata))


mydata<-intercorrs[intercorrs$Part == "Anterior" & intercorrs$Stream == "Dorsal",]
c2<-rep(rep(c(-1, 1), each = 24), times = 1)
c1<-rep(rep(c(1, -1), each = 24), times = 1)
mydata$c2<- c2
mydata$c1<- c1
anova(lm(Correlation ~ c1 + c2, mydata))

mydata<-intercorrs[intercorrs$Part == "Anterior" & intercorrs$Stream == "Ventral",]
c2<-rep(rep(c(-1, 1), each = 24), times = 1)
c1<-rep(rep(c(1, -1), each = 24), times = 1)
mydata$c2<- c2
mydata$c1<- c1
anova(lm(Correlation ~ c1 + c2, mydata))

mydata<-intercorrs[intercorrs$Part == "Posterior" & intercorrs$Stream == "Dorsal",]
c2<-rep(rep(c(-1, 1), each = 24), times = 1)
c1<-rep(rep(c(1, -1), each = 24), times = 1)
mydata$c2<- c2
mydata$c1<- c1
anova(lm(Correlation ~ c1 + c2, mydata))

mydata<-intercorrs[intercorrs$Part == "Posterior" & intercorrs$Stream == "Ventral",]
c2<-rep(rep(c(-1, 1), each = 24), times = 1)
c1<-rep(rep(c(1, -1), each = 24), times = 1)
mydata$c2<- c2
mydata$c1<- c1
anova(lm(Correlation ~ c1 + c2, mydata))
