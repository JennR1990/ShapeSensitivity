ANOVAanalysis<- function(AllDataANOVA){
  AllDataANOVA$ID<- as.factor(AllDataANOVA$ID)
  AllDataANOVA$Object_Type<- as.factor(AllDataANOVA$Object_Type)
  AllDataANOVA$Component_Value<- as.factor(AllDataANOVA$Component_Value)
  AllDataANOVA$Measurement_Type<- as.factor(AllDataANOVA$Measurement_Type)
  AllDataANOVA$ROI_Location<- as.factor(AllDataANOVA$ROI_Location)
  fullmodel <- ezANOVA(data=AllDataANOVA,
                       dv=Betas,
                       wid=ID,
                       within=.(Component_Value,ROI_Location),
                       between = Object_Type,
                       type=3,
                       return_aov=TRUE)
  return(fullmodel)
}

ANOVAanalysis1<- function(AllDataANOVA){
  AllDataANOVA$ID<- as.factor(AllDataANOVA$ID)
  AllDataANOVA$Object_Type<- as.factor(AllDataANOVA$Object_Type)
  AllDataANOVA$Component_Value<- as.factor(AllDataANOVA$Component_Value)
  AllDataANOVA$Measurement_Type<- as.factor(AllDataANOVA$Measurement_Type)
  AllDataANOVA$ROI_Location<- as.factor(AllDataANOVA$ROI_Location)
  fullmodel <- ezANOVA(data=AllDataANOVA,
                       dv=Betas,
                       wid=ID,
                       within=.(Component_Value,ROI_Location,Object_Type),
                       type=3,
                       return_aov=TRUE)
  return(fullmodel)
}