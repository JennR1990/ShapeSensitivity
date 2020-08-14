Sens<- read.csv('Data/Shape sen for each component_ANOVA.csv', header = TRUE)
Comps<- read.csv('Data/Components_ANOVA_format.csv', header = TRUE)
Sens$Component_Value<- as.factor(Sens$Component_Value)
Comps$Component_Value<- as.factor(Comps$Component_Value)

  library('ez')
fullmodel <- ezANOVA(data=Sens,
                       dv=Betas,
                       wid=ID,
                       within=.(Component_Value, Pathway, Hemi, Object_Type),
                       type=3,
                       return_aov=TRUE)
fullmodel

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
                     within=.(Component_Value, Pathway, Hemi, Object_Type),
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

intercorrs <- read.table("Data/intersubjectcorrelation_ANOVA.xlsx", 
                         col_types = c("text", "numeric", "text", 
                                       "text", "text", "text", "text"))


intercorrs$Subject<- as.factor(intercorrs$Subject)
intercorrs$Object<- as.factor(intercorrs$Object)
intercorrs$Stream<- as.factor(intercorrs$Stream)
intercorrs$Hemisphere<- as.factor(intercorrs$Hemisphere)
intercorrs$Part<- as.factor(intercorrs$Part)
fullmodel1 <- ezANOVA(data=intercorrs,
                     dv=Correlation,
                     wid=Subject,
                     within = c(Object, Stream, Hemisphere, Part),
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
