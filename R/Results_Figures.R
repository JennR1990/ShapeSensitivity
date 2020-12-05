novelShapesen<- function (){
  
  v1mean<-mean(c(Sens$Betas[Sens$ROI_Location == 'Ventral_R' & Sens$Component_Value == 1 & Sens$Object_Type == 'Novel'],Sens$Betas[Sens$ROI_Location == 'Ventral_L' & Sens$Component_Value == 1 & Sens$Object_Type == 'Novel'])) 
  d1mean<-mean(c(Sens$Betas[Sens$ROI_Location == 'Dorsal_L' & Sens$Component_Value == 1 & Sens$Object_Type == 'Novel'], Sens$Betas[Sens$ROI_Location == 'Dorsal_R' & Sens$Component_Value == 1 & Sens$Object_Type == 'Novel' ]))
  v2mean<-mean(c(Sens$Betas[Sens$ROI_Location == 'Ventral_R' & Sens$Component_Value == 2 & Sens$Object_Type == 'Novel'],Sens$Betas[Sens$ROI_Location == 'Ventral_L' & Sens$Component_Value == 2 & Sens$Object_Type == 'Novel']))
  d2mean<-mean(c(Sens$Betas[Sens$ROI_Location == 'Dorsal_L' & Sens$Component_Value == 2 & Sens$Object_Type == 'Novel' ], Sens$Betas[Sens$ROI_Location == 'Dorsal_R' & Sens$Component_Value == 2 & Sens$Object_Type == 'Novel' ]))
  nmeans<- c(d1mean,d2mean, v1mean, v2mean)
  v1se<-sd(c(Sens$Betas[Sens$ROI_Location == 'Ventral_R' & Sens$Component_Value == 1 & Sens$Object_Type == 'Novel'],Sens$Betas[Sens$ROI_Location == 'Ventral_L' & Sens$Component_Value == 1 & Sens$Object_Type == 'Novel']))/sqrt(24) 
  d1se<-sd(c(Sens$Betas[Sens$ROI_Location == 'Dorsal_L' & Sens$Component_Value == 1 & Sens$Object_Type == 'Novel'], Sens$Betas[Sens$ROI_Location == 'Dorsal_R' & Sens$Component_Value == 1 & Sens$Object_Type == 'Novel' ]))/sqrt(24)
  v2se<-sd(c(Sens$Betas[Sens$ROI_Location == 'Ventral_R' & Sens$Component_Value == 2 & Sens$Object_Type == 'Novel'],Sens$Betas[Sens$ROI_Location == 'Ventral_L' & Sens$Component_Value == 2 & Sens$Object_Type == 'Novel']))/sqrt(24)
  d2se<-sd(c(Sens$Betas[Sens$ROI_Location == 'Dorsal_L' & Sens$Component_Value == 2 & Sens$Object_Type == 'Novel' ], Sens$Betas[Sens$ROI_Location == 'Dorsal_R' & Sens$Component_Value == 2 & Sens$Object_Type == 'Novel' ]))/sqrt(24)

  nses<- c(d1se,d2se,v1se, v2se)
  
  midpoints<-barplot(nmeans,main = "Novel", col = c('deepskyblue3', 'deepskyblue','deepskyblue3', 'deepskyblue'), ylim = c(-0,.20), 
                     axes = FALSE, space = c(0,0,0.5,0))
  legend(.5,0.18, legend = c('Component 1', 'Component 2'), fill = c('deepskyblue3', 'deepskyblue') )
  axis(1, at=c(1,3.5), labels = c('Dorsal', 'Ventral'))
  axis(2, at= c(0,.05,.10,.15), labels = c(0,.05,.10,.15), las = 1)
  arrows(x0 = midpoints, y0=nmeans - nses, x1 = midpoints, y1 = nmeans + nses, code = 3, angle = 90, length = .1)
}

semanticShapesen<- function (){
  
  
  v1mean<-mean(c(Sens$Betas[Sens$ROI_Location == 'Ventral_R' & Sens$Component_Value == 1 & Sens$Object_Type == 'Semantic'],Sens$Betas[Sens$ROI_Location == 'Ventral_L' & Sens$Component_Value == 1 & Sens$Object_Type == 'Semantic'])) 
  d1mean<-mean(c(Sens$Betas[Sens$ROI_Location == 'Dorsal_L' & Sens$Component_Value == 1 & Sens$Object_Type == 'Semantic'], Sens$Betas[Sens$ROI_Location == 'Dorsal_R' & Sens$Component_Value == 1 & Sens$Object_Type == 'Semantic' ]))
  v2mean<-mean(c(Sens$Betas[Sens$ROI_Location == 'Ventral_R' & Sens$Component_Value == 2 & Sens$Object_Type == 'Semantic'],Sens$Betas[Sens$ROI_Location == 'Ventral_L' & Sens$Component_Value == 2 & Sens$Object_Type == 'Semantic']))
  d2mean<-mean(c(Sens$Betas[Sens$ROI_Location == 'Dorsal_L' & Sens$Component_Value == 2 & Sens$Object_Type == 'Semantic' ], Sens$Betas[Sens$ROI_Location == 'Dorsal_R' & Sens$Component_Value == 2 & Sens$Object_Type == 'Semantic' ]))
  means<- c(d1mean,d2mean, v1mean, v2mean)
  v1se<-sd(c(Sens$Betas[Sens$ROI_Location == 'Ventral_R' & Sens$Component_Value == 1 & Sens$Object_Type == 'Semantic'],Sens$Betas[Sens$ROI_Location == 'Ventral_L' & Sens$Component_Value == 1 & Sens$Object_Type == 'Semantic']))/sqrt(24) 
  d1se<-sd(c(Sens$Betas[Sens$ROI_Location == 'Dorsal_L' & Sens$Component_Value == 1 & Sens$Object_Type == 'Semantic'], Sens$Betas[Sens$ROI_Location == 'Dorsal_R' & Sens$Component_Value == 1 & Sens$Object_Type == 'Semantic' ]))/sqrt(24)
  v2se<-sd(c(Sens$Betas[Sens$ROI_Location == 'Ventral_R' & Sens$Component_Value == 2 & Sens$Object_Type == 'Semantic'],Sens$Betas[Sens$ROI_Location == 'Ventral_L' & Sens$Component_Value == 2 & Sens$Object_Type == 'Semantic']))/sqrt(24)
  d2se<-sd(c(Sens$Betas[Sens$ROI_Location == 'Dorsal_L' & Sens$Component_Value == 2 & Sens$Object_Type == 'Semantic' ], Sens$Betas[Sens$ROI_Location == 'Dorsal_R' & Sens$Component_Value == 2 & Sens$Object_Type == 'Semantic' ]))/sqrt(24)
  
  ses<- c(d1se,d2se,v1se, v2se)

  midpoints<-barplot(means,main = "Semantic", col = c('darkorchid4', 'darkorchid1','darkorchid4', 'darkorchid1','darkorchid4', 'darkorchid1'), ylim = c(-0,.20), 
                     axes = FALSE, space = c(0,0,.5,0))
  legend(.5,0.18, legend = c('Component 1', 'Component 2'), fill = c('darkorchid4', 'darkorchid1') )
  axis(1, at=c(1,3.5), labels = c('Dorsal',  'Ventral'))
  axis(2, at= c(0,.05,.10,.15), labels = c(0,.05,.10,.15), las = 1)
  arrows(x0 = midpoints, y0=means - ses, x1 = midpoints, y1 = means + ses, code = 3, angle = 90, length = .1)
}



novelComponents<- function (){
  
  v1mean<-mean(c(Comps$Betas[Comps$ROI_Location == 'Ventral_R' & Comps$Component_Value == 1 & Comps$Object_Type == 'Novel'],Comps$Betas[Comps$ROI_Location == 'Ventral_L' & Comps$Component_Value == 1 & Comps$Object_Type == 'Novel'])) 
  d1mean<-mean(c(Comps$Betas[Comps$ROI_Location == 'Dorsal_L' & Comps$Component_Value == 1 & Comps$Object_Type == 'Novel'], Comps$Betas[Comps$ROI_Location == 'Dorsal_R' & Comps$Component_Value == 1 & Comps$Object_Type == 'Novel' ]))
  v2mean<-mean(c(Comps$Betas[Comps$ROI_Location == 'Ventral_R' & Comps$Component_Value == 2 & Comps$Object_Type == 'Novel'],Comps$Betas[Comps$ROI_Location == 'Ventral_L' & Comps$Component_Value == 2 & Comps$Object_Type == 'Novel']))
  d2mean<-mean(c(Comps$Betas[Comps$ROI_Location == 'Dorsal_L' & Comps$Component_Value == 2 & Comps$Object_Type == 'Novel' ], Comps$Betas[Comps$ROI_Location == 'Dorsal_R' & Comps$Component_Value == 2 & Comps$Object_Type == 'Novel' ]))
  nmeans<- c(d1mean,d2mean, v1mean, v2mean)
  v1se<-sd(c(Comps$Betas[Comps$ROI_Location == 'Ventral_R' & Comps$Component_Value == 1 & Comps$Object_Type == 'Novel'],Comps$Betas[Comps$ROI_Location == 'Ventral_L' & Comps$Component_Value == 1 & Comps$Object_Type == 'Novel']))/sqrt(24) 
  d1se<-sd(c(Comps$Betas[Comps$ROI_Location == 'Dorsal_L' & Comps$Component_Value == 1 & Comps$Object_Type == 'Novel'], Comps$Betas[Comps$ROI_Location == 'Dorsal_R' & Comps$Component_Value == 1 & Comps$Object_Type == 'Novel' ]))/sqrt(24)
  v2se<-sd(c(Comps$Betas[Comps$ROI_Location == 'Ventral_R' & Comps$Component_Value == 2 & Comps$Object_Type == 'Novel'],Comps$Betas[Comps$ROI_Location == 'Ventral_L' & Comps$Component_Value == 2 & Comps$Object_Type == 'Novel']))/sqrt(24)
  d2se<-sd(c(Comps$Betas[Comps$ROI_Location == 'Dorsal_L' & Comps$Component_Value == 2 & Comps$Object_Type == 'Novel' ], Comps$Betas[Comps$ROI_Location == 'Dorsal_R' & Comps$Component_Value == 2 & Comps$Object_Type == 'Novel' ]))/sqrt(24)
  
  nses<- c(d1se,d2se,v1se, v2se)
  
  midpoints<-barplot(nmeans,main = "Novel", col = c('deepskyblue3', 'deepskyblue','deepskyblue3', 'deepskyblue'), ylim = c(-1,1), 
                     axes = FALSE, space = c(0,0,0.5,0))
  legend(2.5,-.5, legend = c('Component 1', 'Component 2'), fill = c('deepskyblue3', 'deepskyblue') )
  axis(1, at=c(1,3.5), labels = c('Dorsal', 'Ventral'))
  axis(2, at= c(-1,0,1), labels = c(-1,0,1), las = 1)
  arrows(x0 = midpoints, y0=nmeans - nses, x1 = midpoints, y1 = nmeans + nses, code = 3, angle = 90, length = .1)
}

semanticComponents<- function (){
  
  
  v1mean<-mean(c(Comps$Betas[Comps$ROI_Location == 'Ventral_R' & Comps$Component_Value == 1 & Comps$Object_Type == 'Semantic'],Comps$Betas[Comps$ROI_Location == 'Ventral_L' & Comps$Component_Value == 1 & Comps$Object_Type == 'Semantic'])) 
  d1mean<-mean(c(Comps$Betas[Comps$ROI_Location == 'Dorsal_L' & Comps$Component_Value == 1 & Comps$Object_Type == 'Semantic'], Comps$Betas[Comps$ROI_Location == 'Dorsal_R' & Comps$Component_Value == 1 & Comps$Object_Type == 'Semantic' ]))
  v2mean<-mean(c(Comps$Betas[Comps$ROI_Location == 'Ventral_R' & Comps$Component_Value == 2 & Comps$Object_Type == 'Semantic'],Comps$Betas[Comps$ROI_Location == 'Ventral_L' & Comps$Component_Value == 2 & Comps$Object_Type == 'Semantic']))
  d2mean<-mean(c(Comps$Betas[Comps$ROI_Location == 'Dorsal_L' & Comps$Component_Value == 2 & Comps$Object_Type == 'Semantic' ], Comps$Betas[Comps$ROI_Location == 'Dorsal_R' & Comps$Component_Value == 2 & Comps$Object_Type == 'Semantic' ]))
  means<- c(d1mean,d2mean, v1mean, v2mean)
  v1se<-sd(c(Comps$Betas[Comps$ROI_Location == 'Ventral_R' & Comps$Component_Value == 1 & Comps$Object_Type == 'Semantic'],Comps$Betas[Comps$ROI_Location == 'Ventral_L' & Comps$Component_Value == 1 & Comps$Object_Type == 'Semantic']))/sqrt(24) 
  d1se<-sd(c(Comps$Betas[Comps$ROI_Location == 'Dorsal_L' & Comps$Component_Value == 1 & Comps$Object_Type == 'Semantic'], Comps$Betas[Comps$ROI_Location == 'Dorsal_R' & Comps$Component_Value == 1 & Comps$Object_Type == 'Semantic' ]))/sqrt(24)
  v2se<-sd(c(Comps$Betas[Comps$ROI_Location == 'Ventral_R' & Comps$Component_Value == 2 & Comps$Object_Type == 'Semantic'],Comps$Betas[Comps$ROI_Location == 'Ventral_L' & Comps$Component_Value == 2 & Comps$Object_Type == 'Semantic']))/sqrt(24)
  d2se<-sd(c(Comps$Betas[Comps$ROI_Location == 'Dorsal_L' & Comps$Component_Value == 2 & Comps$Object_Type == 'Semantic' ], Comps$Betas[Comps$ROI_Location == 'Dorsal_R' & Comps$Component_Value == 2 & Comps$Object_Type == 'Semantic' ]))/sqrt(24)
  
  ses<- c(d1se,d2se,v1se, v2se)
  
  midpoints<-barplot(means,main = "Semantic", col = c('darkorchid4', 'darkorchid1','darkorchid4', 'darkorchid1','darkorchid4', 'darkorchid1'), ylim = c(-1,1), 
                     axes = FALSE, space = c(0,0,.5,0))
  legend(2.5,-.5, legend = c('Component 1', 'Component 2'), fill = c('darkorchid4', 'darkorchid1') )
  axis(1, at=c(1,3.5), labels = c('Dorsal',  'Ventral'))
  axis(2, at= c(-1,0,1), labels = c(-1,0,1), las = 1)
  arrows(x0 = midpoints, y0=means - ses, x1 = midpoints, y1 = means + ses, code = 3, angle = 90, length = .1)
}



svglite(file='figures/Sensitivities and Components.svg', width=12, height=8, system_fonts=list(sans = "Arial"))
layout(matrix(c(1,2,3,4), nrow=2, byrow=TRUE), heights=c(1)) 
semanticComponents()
novelComponents()
semanticShapesen()
novelShapesen()
dev.off()
