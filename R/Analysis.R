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

UpperC<- 

fullmodel <- ezANOVA(data=Comps,
                     dv=Betas,
                     wid=ID,
                     within=.(Component_Value, Pathway, Hemi, Object_Type),
                     type=3,
                     return_aov=TRUE)
fullmodel



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


