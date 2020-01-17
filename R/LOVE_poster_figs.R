##BarPlots for LOVE poster


#I want to make plots of the average beta? not sure what this value is- for each ROI and each component

novelcomponentsensitivity<- function (){

vrcomp1mean<-mean(components$Betas[components$ROI_Location == 'Ventral_R' & components$Component_Value == 1 & components$Object_Type == 'Novel' & components$Measurement_Type == 'Components'])
vrcomp1se<-sd(components$Betas[components$ROI_Location == 'Ventral_R' & components$Component_Value == 1 & components$Object_Type == 'Novel' & components$Measurement_Type == 'Components'])/sqrt(12)
vlcomp1mean<-mean(components$Betas[components$ROI_Location == 'Ventral_L' & components$Component_Value == 1 & components$Object_Type == 'Novel' & components$Measurement_Type == 'Components'])
dlcomp1mean<-mean(components$Betas[components$ROI_Location == 'Dorsal_L' & components$Component_Value == 1 & components$Object_Type == 'Novel' & components$Measurement_Type == 'Components'])
drcomp1mean<-mean(components$Betas[components$ROI_Location == 'Dorsal_R' & components$Component_Value == 1 & components$Object_Type == 'Novel' & components$Measurement_Type == 'Components'])
vrcomp2mean<-mean(components$Betas[components$ROI_Location == 'Ventral_R' & components$Component_Value == 2 & components$Object_Type == 'Novel' & components$Measurement_Type == 'Components'])
vlcomp2mean<-mean(components$Betas[components$ROI_Location == 'Ventral_L' & components$Component_Value == 2 & components$Object_Type == 'Novel' & components$Measurement_Type == 'Components'])
dlcomp2mean<-mean(components$Betas[components$ROI_Location == 'Dorsal_L' & components$Component_Value == 2 & components$Object_Type == 'Novel' & components$Measurement_Type == 'Components'])
drcomp2mean<-mean(components$Betas[components$ROI_Location == 'Dorsal_R' & components$Component_Value == 2 & components$Object_Type == 'Novel' & components$Measurement_Type == 'Components'])
vlcomp1se<-sd(components$Betas[components$ROI_Location == 'Ventral_L' & components$Component_Value == 1 & components$Object_Type == 'Novel' & components$Measurement_Type == 'Components'])/sqrt(12)
dlcomp1se<-sd(components$Betas[components$ROI_Location == 'Dorsal_L' & components$Component_Value == 1 & components$Object_Type == 'Novel' & components$Measurement_Type == 'Components'])/sqrt(12)
drcomp1se<-sd(components$Betas[components$ROI_Location == 'Dorsal_R' & components$Component_Value == 1 & components$Object_Type == 'Novel' & components$Measurement_Type == 'Components'])/sqrt(12)
vrcomp2se<-sd(components$Betas[components$ROI_Location == 'Ventral_R' & components$Component_Value == 2 & components$Object_Type == 'Novel' & components$Measurement_Type == 'Components'])/sqrt(12)
vlcomp2se<-sd(components$Betas[components$ROI_Location == 'Ventral_L' & components$Component_Value == 2 & components$Object_Type == 'Novel' & components$Measurement_Type == 'Components'])/sqrt(12)
dlcomp2se<-sd(components$Betas[components$ROI_Location == 'Dorsal_L' & components$Component_Value == 2 & components$Object_Type == 'Novel' & components$Measurement_Type == 'Components'])/sqrt(12)
drcomp2se<-sd(components$Betas[components$ROI_Location == 'Dorsal_R' & components$Component_Value == 2 & components$Object_Type == 'Novel' & components$Measurement_Type == 'Components'])/sqrt(12)

means<- c(dlcomp1mean,dlcomp2mean, drcomp1mean, drcomp2mean, vlcomp1mean, vlcomp2mean, vrcomp1mean, vrcomp2mean)
ses<- c(dlcomp1se,dlcomp2se, drcomp1se, drcomp2se, vlcomp1se, vlcomp2se, vrcomp1se, vrcomp2se)
#midpoints<-barplot(means, space = 0,main = "Novel", col = c('black', 'grey','black', 'grey','black', 'grey'), ylim = c(-1,1.20), xlim = c(0,8), axes = FALSE, width = 1)
midpoints<-barplot(means, space = 0,main = "Novel", col = c('deepskyblue3', 'deepskyblue','deepskyblue3', 'deepskyblue','deepskyblue3', 'deepskyblue'), ylim = c(-1,1.20), xlim = c(0,8), axes = FALSE, width = 1)
#barplot(means, space = 0,main = "Novel", col = c('black', 'grey','black', 'grey','black', 'grey'), ylim = c(-1,1.20), xlim = c(0,8), axes = FALSE, width = 1)
#legend(6,-0.6, legend = c('Component 1', 'Component 2'), fill = c('black', 'grey') )
legend(5,-0.6, legend = c('Component 1', 'Component 2'), fill = c('deepskyblue3', 'deepskyblue') )
axis(1, at=c(1,3,5,7), labels = c('Dorsal left', 'Dorsal right', 'Ventral left', 'Ventral right'))
axis(2, at= c(-1,0,1), labels = c(-1,0,1), las = 1)
arrows(x0 = midpoints, y0=means - ses,  x1 = midpoints, y1 = means + ses, code = 3, angle = 90, length = .1)
}

semanticcomponentsensitivity<- function (){
  
  vrcomp1mean<-mean(components$Betas[components$ROI_Location == 'Ventral_R' & components$Component_Value == 1 & components$Object_Type == 'Semantic' & components$Measurement_Type == 'Components'])
  vlcomp1mean<-mean(components$Betas[components$ROI_Location == 'Ventral_L' & components$Component_Value == 1 & components$Object_Type == 'Semantic' & components$Measurement_Type == 'Components'])
  dlcomp1mean<-mean(components$Betas[components$ROI_Location == 'Dorsal_L' & components$Component_Value == 1 & components$Object_Type == 'Semantic' & components$Measurement_Type == 'Components'])
  drcomp1mean<-mean(components$Betas[components$ROI_Location == 'Dorsal_R' & components$Component_Value == 1 & components$Object_Type == 'Semantic' & components$Measurement_Type == 'Components'])
  vrcomp2mean<-mean(components$Betas[components$ROI_Location == 'Ventral_R' & components$Component_Value == 2 & components$Object_Type == 'Semantic' & components$Measurement_Type == 'Components'])
  vlcomp2mean<-mean(components$Betas[components$ROI_Location == 'Ventral_L' & components$Component_Value == 2 & components$Object_Type == 'Semantic' & components$Measurement_Type == 'Components'])
  dlcomp2mean<-mean(components$Betas[components$ROI_Location == 'Dorsal_L' & components$Component_Value == 2 & components$Object_Type == 'Semantic' & components$Measurement_Type == 'Components'])
  drcomp2mean<-mean(components$Betas[components$ROI_Location == 'Dorsal_R' & components$Component_Value == 2 & components$Object_Type == 'Semantic' & components$Measurement_Type == 'Components'])
  means<- c(dlcomp1mean,dlcomp2mean, drcomp1mean, drcomp2mean, vlcomp1mean, vlcomp2mean, vrcomp1mean, vrcomp2mean)
  vrcomp1se<-sd(components$Betas[components$ROI_Location == 'Ventral_R' & components$Component_Value == 1 & components$Object_Type == 'Semantic' & components$Measurement_Type == 'Components'])/sqrt(12)
  vlcomp1se<-sd(components$Betas[components$ROI_Location == 'Ventral_L' & components$Component_Value == 1 & components$Object_Type == 'Semantic' & components$Measurement_Type == 'Components'])/sqrt(12)
  dlcomp1se<-sd(components$Betas[components$ROI_Location == 'Dorsal_L' & components$Component_Value == 1 & components$Object_Type == 'Semantic' & components$Measurement_Type == 'Components'])/sqrt(12)
  drcomp1se<-sd(components$Betas[components$ROI_Location == 'Dorsal_R' & components$Component_Value == 1 & components$Object_Type == 'Semantic' & components$Measurement_Type == 'Components'])/sqrt(12)
  vrcomp2se<-sd(components$Betas[components$ROI_Location == 'Ventral_R' & components$Component_Value == 2 & components$Object_Type == 'Semantic' & components$Measurement_Type == 'Components'])/sqrt(12)
  vlcomp2se<-sd(components$Betas[components$ROI_Location == 'Ventral_L' & components$Component_Value == 2 & components$Object_Type == 'Semantic' & components$Measurement_Type == 'Components'])/sqrt(12)
  dlcomp2se<-sd(components$Betas[components$ROI_Location == 'Dorsal_L' & components$Component_Value == 2 & components$Object_Type == 'Semantic' & components$Measurement_Type == 'Components'])/sqrt(12)
  drcomp2se<-sd(components$Betas[components$ROI_Location == 'Dorsal_R' & components$Component_Value == 2 & components$Object_Type == 'Semantic' & components$Measurement_Type == 'Components'])/sqrt(12)
  
  
  
  #midpoints<-barplot(means, space = 0,main = "Semantic", col = c('black', 'grey','black', 'grey','black', 'grey'), ylim = c(-1,1.2), xlim = c(0,8), axes = FALSE, width = 1)
  midpoints<-barplot(means, space = 0,main = "Semantic", col = c('darkorchid4', 'darkorchid1','darkorchid4', 'darkorchid1','darkorchid4', 'darkorchid1'), ylim =  c(-1,1.2), xlim = c(0,8), axes = FALSE, width = 1)
  #barplot(means, space = 0,main = "Semantic", col = c('black', 'grey','black', 'grey','black', 'grey'), ylim = c(-1,1.2), xlim = c(0,8), axes = FALSE, width = 1)
  #legend(6,-0.6, legend = c('Component 1', 'Component 2'), fill = c('black', 'grey') )
  legend(5,-0.6, legend = c('Component 1', 'Component 2'), fill = c('darkorchid4', 'darkorchid1') )
  axis(1, at=c(1,3,5,7), labels = c('Dorsal left', 'Dorsal right', 'Ventral left', 'Ventral right'))
  axis(2, at= c(-1,0,1), labels = c(-1,0,1), las = 1)
  ses<- c(dlcomp1se,dlcomp2se, drcomp1se, drcomp2se, vlcomp1se, vlcomp2se, vrcomp1se, vrcomp2se)
  arrows(x0 = midpoints, y0=means - ses, x1 = midpoints, y1 = means + ses, code = 3, angle = 90, length = .1)
}


novelShapesen_sensitivity<- function (){
  
  vrcomp1mean<-mean(components$Betas[components$ROI_Location == 'Ventral_R' & components$Component_Value == 1 & components$Object_Type == 'Novel' & components$Measurement_Type == 'Sensitivity_limited'])
  vlcomp1mean<-mean(components$Betas[components$ROI_Location == 'Ventral_L' & components$Component_Value == 1 & components$Object_Type == 'Novel' & components$Measurement_Type == 'Sensitivity_limited'])
  dlcomp1mean<-mean(components$Betas[components$ROI_Location == 'Dorsal_L' & components$Component_Value == 1 & components$Object_Type == 'Novel' & components$Measurement_Type == 'Sensitivity_limited'])
  drcomp1mean<-mean(components$Betas[components$ROI_Location == 'Dorsal_R' & components$Component_Value == 1 & components$Object_Type == 'Novel' & components$Measurement_Type == 'Sensitivity_limited'])
  vrcomp2mean<-mean(components$Betas[components$ROI_Location == 'Ventral_R' & components$Component_Value == 2 & components$Object_Type == 'Novel' & components$Measurement_Type == 'Sensitivity_limited'])
  vlcomp2mean<-mean(components$Betas[components$ROI_Location == 'Ventral_L' & components$Component_Value == 2 & components$Object_Type == 'Novel' & components$Measurement_Type == 'Sensitivity_limited'])
  dlcomp2mean<-mean(components$Betas[components$ROI_Location == 'Dorsal_L' & components$Component_Value == 2 & components$Object_Type == 'Novel' & components$Measurement_Type == 'Sensitivity_limited'])
  drcomp2mean<-mean(components$Betas[components$ROI_Location == 'Dorsal_R' & components$Component_Value == 2 & components$Object_Type == 'Novel' & components$Measurement_Type == 'Sensitivity_limited'])
  means<- c(dlcomp1mean,dlcomp2mean, drcomp1mean, drcomp2mean, vlcomp1mean, vlcomp2mean, vrcomp1mean, vrcomp2mean)
  vrcomp1se<-sd(components$Betas[components$ROI_Location == 'Ventral_R' & components$Component_Value == 1 & components$Object_Type == 'Novel' & components$Measurement_Type == 'Sensitivity_limited'])/sqrt(12)
  vlcomp1se<-sd(components$Betas[components$ROI_Location == 'Ventral_L' & components$Component_Value == 1 & components$Object_Type == 'Novel' & components$Measurement_Type == 'Sensitivity_limited'])/sqrt(12)
  dlcomp1se<-sd(components$Betas[components$ROI_Location == 'Dorsal_L' & components$Component_Value == 1 & components$Object_Type == 'Novel' & components$Measurement_Type == 'Sensitivity_limited'])/sqrt(12)
  drcomp1se<-sd(components$Betas[components$ROI_Location == 'Dorsal_R' & components$Component_Value == 1 & components$Object_Type == 'Novel' & components$Measurement_Type == 'Sensitivity_limited'])/sqrt(12)
  vrcomp2se<-sd(components$Betas[components$ROI_Location == 'Ventral_R' & components$Component_Value == 2 & components$Object_Type == 'Novel' & components$Measurement_Type == 'Sensitivity_limited'])/sqrt(12)
  vlcomp2se<-sd(components$Betas[components$ROI_Location == 'Ventral_L' & components$Component_Value == 2 & components$Object_Type == 'Novel' & components$Measurement_Type == 'Sensitivity_limited'])/sqrt(12)
  dlcomp2se<-sd(components$Betas[components$ROI_Location == 'Dorsal_L' & components$Component_Value == 2 & components$Object_Type == 'Novel' & components$Measurement_Type == 'Sensitivity_limited'])/sqrt(12)
  drcomp2se<-sd(components$Betas[components$ROI_Location == 'Dorsal_R' & components$Component_Value == 2 & components$Object_Type == 'Novel' & components$Measurement_Type == 'Sensitivity_limited'])/sqrt(12)
  ses<- c(dlcomp1se,dlcomp2se, drcomp1se, drcomp2se, vlcomp1se, vlcomp2se, vrcomp1se, vrcomp2se)
  
  
  
  #barplot(means,main = "Novel", col = c('black', 'grey','black', 'grey','black', 'grey'), ylim = c(-0,.20), 
         # axes = FALSE, space = c(0,0,.5, 0, .5,0,.5,0))
  midpoints<-barplot(means,main = "Novel", col = c('deepskyblue3', 'deepskyblue','deepskyblue3', 'deepskyblue','deepskyblue3', 'deepskyblue'), ylim = c(-0,.20), 
          axes = FALSE, space = c(0,0,.5, 0, .5,0,.5,0))
  #legend(.5,0.17, legend = c('Component 1', 'Component 2'), fill = c('black', 'grey') )
  legend(.5,0.17, legend = c('Component 1', 'Component 2'), fill = c('deepskyblue3', 'deepskyblue') )
  axis(1, at=c(1,3.5,6,8.5), labels = c('Dorsal left', 'Dorsal right', 'Ventral left', 'Ventral right'))
  axis(2, at= c(0,.05,.10,.15), labels = c(0,.05,.10,.15), las = 1)
  arrows(x0 = midpoints, y0=means - ses, x1 = midpoints, y1 = means + ses, code = 3, angle = 90, length = .1)
}

semanticShapesen_sensitivity<- function (){
  
  vrcomp1mean<-mean(components$Betas[components$ROI_Location == 'Ventral_R' & components$Component_Value == 1 & components$Object_Type == 'Semantic' & components$Measurement_Type == 'Sensitivity_limited'])
  vlcomp1mean<-mean(components$Betas[components$ROI_Location == 'Ventral_L' & components$Component_Value == 1 & components$Object_Type == 'Semantic' & components$Measurement_Type == 'Sensitivity_limited'])
  dlcomp1mean<-mean(components$Betas[components$ROI_Location == 'Dorsal_L' & components$Component_Value == 1 & components$Object_Type == 'Semantic' & components$Measurement_Type == 'Sensitivity_limited'])
  drcomp1mean<-mean(components$Betas[components$ROI_Location == 'Dorsal_R' & components$Component_Value == 1 & components$Object_Type == 'Semantic' & components$Measurement_Type == 'Sensitivity_limited'])
  vrcomp2mean<-mean(components$Betas[components$ROI_Location == 'Ventral_R' & components$Component_Value == 2 & components$Object_Type == 'Semantic' & components$Measurement_Type == 'Sensitivity_limited'])
  vlcomp2mean<-mean(components$Betas[components$ROI_Location == 'Ventral_L' & components$Component_Value == 2 & components$Object_Type == 'Semantic' & components$Measurement_Type == 'Sensitivity_limited'])
  dlcomp2mean<-mean(components$Betas[components$ROI_Location == 'Dorsal_L' & components$Component_Value == 2 & components$Object_Type == 'Semantic' & components$Measurement_Type == 'Sensitivity_limited'])
  drcomp2mean<-mean(components$Betas[components$ROI_Location == 'Dorsal_R' & components$Component_Value == 2 & components$Object_Type == 'Semantic' & components$Measurement_Type == 'Sensitivity_limited'])
  means<- c(dlcomp1mean,dlcomp2mean, drcomp1mean, drcomp2mean, vlcomp1mean, vlcomp2mean, vrcomp1mean, vrcomp2mean)
  vrcomp1se<-sd(components$Betas[components$ROI_Location == 'Ventral_R' & components$Component_Value == 1 & components$Object_Type == 'Semantic' & components$Measurement_Type == 'Sensitivity_limited'])/sqrt(12)
  vlcomp1se<-sd(components$Betas[components$ROI_Location == 'Ventral_L' & components$Component_Value == 1 & components$Object_Type == 'Semantic' & components$Measurement_Type == 'Sensitivity_limited'])/sqrt(12)
  dlcomp1se<-sd(components$Betas[components$ROI_Location == 'Dorsal_L' & components$Component_Value == 1 & components$Object_Type == 'Semantic' & components$Measurement_Type == 'Sensitivity_limited'])/sqrt(12)
  drcomp1se<-sd(components$Betas[components$ROI_Location == 'Dorsal_R' & components$Component_Value == 1 & components$Object_Type == 'Semantic' & components$Measurement_Type == 'Sensitivity_limited'])/sqrt(12)
  vrcomp2se<-sd(components$Betas[components$ROI_Location == 'Ventral_R' & components$Component_Value == 2 & components$Object_Type == 'Semantic' & components$Measurement_Type == 'Sensitivity_limited'])/sqrt(12)
  vlcomp2se<-sd(components$Betas[components$ROI_Location == 'Ventral_L' & components$Component_Value == 2 & components$Object_Type == 'Semantic' & components$Measurement_Type == 'Sensitivity_limited'])/sqrt(12)
  dlcomp2se<-sd(components$Betas[components$ROI_Location == 'Dorsal_L' & components$Component_Value == 2 & components$Object_Type == 'Semantic' & components$Measurement_Type == 'Sensitivity_limited'])/sqrt(12)
  drcomp2se<-sd(components$Betas[components$ROI_Location == 'Dorsal_R' & components$Component_Value == 2 & components$Object_Type == 'Semantic' & components$Measurement_Type == 'Sensitivity_limited'])/sqrt(12)
  ses<- c(dlcomp1se,dlcomp2se, drcomp1se, drcomp2se, vlcomp1se, vlcomp2se, vrcomp1se, vrcomp2se)
  
  # barplot(means,main = "Semantic", col = c('black', 'grey','black', 'grey','black', 'grey'), ylim = c(-0,.20), 
  #         axes = FALSE, space = c(0,0,.5, 0, .5,0,.5,0))
  # legend(.5,0.17, legend = c('Component 1', 'Component 2'), fill = c('black', 'grey') )
  midpoints<-barplot(means,main = "Semantic", col = c('darkorchid4', 'darkorchid1','darkorchid4', 'darkorchid1','darkorchid4', 'darkorchid1'), ylim = c(-0,.20), 
          axes = FALSE, space = c(0,0,.5, 0, .5,0,.5,0))
  legend(.5,0.17, legend = c('Component 1', 'Component 2'), fill = c('darkorchid4', 'darkorchid1') )
  axis(1, at=c(1,3.5,6,8.5), labels = c('Dorsal left', 'Dorsal right', 'Ventral left', 'Ventral right'))
  axis(2, at= c(0,.05,.10,.15), labels = c(0,.05,.10,.15), las = 1)
  arrows(x0 = midpoints, y0=means - ses, x1 = midpoints, y1 = means + ses, code = 3, angle = 90, length = .1)
}




Locofmaxsensitivity<- function (){
  semantic <- read_excel("Data/semantic_component_elbow_loc_sens.xlsx")
  novels <- read_excel("Data/novel_component_elbow_loc_sens.xlsx")
  vrmax<-semantic$Elbow_Y_loc[semantic$ROI_Location == 'ventral_right' & semantic$Elbox_Y_Sensitivity == max(semantic$Elbox_Y_Sensitivity[semantic$ROI_Location == 'ventral_right'])]
  vlmax<-semantic$Elbow_Y_loc[semantic$ROI_Location == 'ventral_left' & semantic$Elbox_Y_Sensitivity == max(semantic$Elbox_Y_Sensitivity[semantic$ROI_Location == 'ventral_left'])]
  drmax<-semantic$Elbow_Y_loc[semantic$ROI_Location == 'dorsal_right' & semantic$Elbox_Y_Sensitivity == max(semantic$Elbox_Y_Sensitivity[semantic$ROI_Location == 'dorsal_right'])]
  dlmax<-semantic$Elbow_Y_loc[semantic$ROI_Location == 'dorsal_left' & semantic$Elbox_Y_Sensitivity == max(semantic$Elbox_Y_Sensitivity[semantic$ROI_Location == 'dorsal_left'])]
  nvrmax<-novels$Elbow_Y_loc[novels$ROI_Location == 'ventral_right' & novels$Elbox_Y_Sensitivity == max(novels$Elbox_Y_Sensitivity[novels$ROI_Location == 'ventral_right'])]
  nvlmax<-novels$Elbow_Y_loc[novels$ROI_Location == 'ventral_left' & novels$Elbox_Y_Sensitivity == max(novels$Elbox_Y_Sensitivity[novels$ROI_Location == 'ventral_left'])]
  ndrmax<-novels$Elbow_Y_loc[novels$ROI_Location == 'dorsal_right' & novels$Elbox_Y_Sensitivity == max(novels$Elbox_Y_Sensitivity[novels$ROI_Location == 'dorsal_right'])]
  ndlmax<-novels$Elbow_Y_loc[novels$ROI_Location == 'dorsal_left' & novels$Elbox_Y_Sensitivity == max(novels$Elbox_Y_Sensitivity[novels$ROI_Location == 'dorsal_left'])]

  vrse<-sd(semantic$Elbow_Y_loc[semantic$ROI_Location == 'ventral_right'])/sqrt(12)
  vlse<-sd(semantic$Elbow_Y_loc[semantic$ROI_Location == 'ventral_left' ])/sqrt(12)
  drse<-sd(semantic$Elbow_Y_loc[semantic$ROI_Location == 'dorsal_right' ])/sqrt(12)
  dlse<-sd(semantic$Elbow_Y_loc[semantic$ROI_Location == 'dorsal_left' ])/sqrt(12)
  nvrse<-sd(novels$Elbow_Y_loc[novels$ROI_Location == 'ventral_right' ])/sqrt(12)
  nvlse<-sd(novels$Elbow_Y_loc[novels$ROI_Location == 'ventral_left' ])/sqrt(12)
  ndrse<-sd(novels$Elbow_Y_loc[novels$ROI_Location == 'dorsal_right'])/sqrt(12)
  ndlse<-sd(novels$Elbow_Y_loc[novels$ROI_Location == 'dorsal_left' ])/sqrt(12)
  
  
  sc<-c(dlmax,ndlmax,drmax,ndrmax,vlmax,nvlmax,vrmax,nvrmax)
  ses<-c(dlse,ndlse,drse,ndrse,vlse,nvlse,vrse,nvrse)
  midpoints<-barplot(sc,main = "Location of max sensitivity", col = c('deepskyblue3','darkorchid4', 'deepskyblue3', 'darkorchid4','deepskyblue3','darkorchid4', 'deepskyblue3','darkorchid4'), ylim = c(-100,0), 
          axes = FALSE, space = c(0,0,.5,0,.5,0,.5,0), width = .5)
  legend(3,-75, legend = c('Familiar', 'Unfamiliar'), fill = c('deepskyblue3', 'darkorchid4') )
  axis(1, at=c(.5,1.75,3, 4.25), labels = c('Dorsal left', 'Dorsal right', 'Ventral left', 'Ventral right'))
  axis(2, at= c(-100,-75,-50,-25,0), labels = c(-100,-75,-50,-25,0), las = 1)
  arrows(x0 = midpoints, y0=sc - ses, x1 = midpoints, y1 = sc + ses, code = 3, angle = 90, length = .1)
}









##I want to make plots of location of maximum sensitivity for each ROI and component
##I want to make a plot of the average spatial correlation for each ROI - dont have it for both components


novelspatialcorrelation<- function (){
nvrsc<- mean(novels$`Spatial_correlation(slope~Yloc)`[novels$ROI_Location == 'ventral_right'])
nvlsc<- mean(novels$`Spatial_correlation(slope~Yloc)`[novels$ROI_Location == 'ventral_left'])
ndlsc<- mean(novels$`Spatial_correlation(slope~Yloc)`[novels$ROI_Location == 'dorsal_left'])
ndrsc<- mean(novels$`Spatial_correlation(slope~Yloc)`[novels$ROI_Location == 'dorsal_right'])
sc<-c(ndlsc,ndrsc,nvlsc,nvrsc)
barplot(sc,main = "Novel Spatial Correlation", col = c('grey', 'grey', 'grey', 'grey'), ylim = c(-.5,.25), 
        axes = FALSE, space = .5, width = .5)
axis(1, at=c(.5,1.25,2, 2.75), labels = c('Dorsal left', 'Dorsal right', 'Ventral left', 'Ventral right'))
axis(2, at= c(-0.5,-.25,0,.25), labels = c(-0.5,-.25,0,.25), las = 1)
}

semanticspatialcorrelation<- function (){
  vrsc<- mean(semantic$`Spatial_correlation(slope~Yloc)`[semantic$ROI_Location == 'ventral_right'])
  vlsc<- mean(semantic$`Spatial_correlation(slope~Yloc)`[semantic$ROI_Location == 'ventral_left'])
  dlsc<- mean(semantic$`Spatial_correlation(slope~Yloc)`[semantic$ROI_Location == 'dorsal_left'])
  drsc<- mean(semantic$`Spatial_correlation(slope~Yloc)`[semantic$ROI_Location == 'dorsal_right'])
  sc<-c(dlsc,drsc,vlsc,vrsc)
  barplot(sc,main = "Semantic Spatial Correlation", col = c('grey', 'grey', 'grey', 'grey'), ylim = c(-.5,.25), 
          axes = FALSE, space = .5, width = .5)
  axis(1, at=c(.5,1.25,2, 2.75), labels = c('Dorsal left', 'Dorsal right', 'Ventral left', 'Ventral right'))
  axis(2, at= c(-0.5,-.25,0,.25), labels = c(-0.5,-.25,0,.25), las = 1)
}


spatialcorrelation<- function (){
  nvrsc<- mean(novels$`Spatial_correlation(slope~Yloc)`[novels$ROI_Location == 'ventral_right'])
  nvlsc<- mean(novels$`Spatial_correlation(slope~Yloc)`[novels$ROI_Location == 'ventral_left'])
  ndlsc<- mean(novels$`Spatial_correlation(slope~Yloc)`[novels$ROI_Location == 'dorsal_left'])
  ndrsc<- mean(novels$`Spatial_correlation(slope~Yloc)`[novels$ROI_Location == 'dorsal_right'])
  vrsc<- mean(semantic$`Spatial_correlation(slope~Yloc)`[semantic$ROI_Location == 'ventral_right'])
  vlsc<- mean(semantic$`Spatial_correlation(slope~Yloc)`[semantic$ROI_Location == 'ventral_left'])
  dlsc<- mean(semantic$`Spatial_correlation(slope~Yloc)`[semantic$ROI_Location == 'dorsal_left'])
  drsc<- mean(semantic$`Spatial_correlation(slope~Yloc)`[semantic$ROI_Location == 'dorsal_right'])
  sc<-c(dlsc,ndlsc,drsc,ndrsc, vlsc,nvlsc,vrsc,nvrsc)
  barplot(sc,main = "Spatial Correlation", col = c('grey','black', 'grey', 'black','grey','black', 'grey','black'), ylim = c(-.5,.25), 
          axes = FALSE, space = c(0,0,.5,0,.5,0,.5,0), width = .5)
  legend(0,0.17, legend = c('Semantic', 'Novel'), fill = c('grey', 'black') )
  axis(1, at=c(.5,1.75,3, 4.25), labels = c('Dorsal left', 'Dorsal right', 'Ventral left', 'Ventral right'))
  axis(2, at= c(-0.5,-.25,0,.25), labels = c(-0.5,-.25,0,.25), las = 1)
}




