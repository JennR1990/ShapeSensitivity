t.test(components$Betas[components$ROI_Location == 'Ventral_R' & components$Component_Value == 1 & components$Object_Type == 'Semantic' & components$Measurement_Type == 'Sensitivity_limited'],
       components$Betas[components$ROI_Location == 'Ventral_R' & components$Component_Value == 2 & components$Object_Type == 'Semantic' & components$Measurement_Type == 'Sensitivity_limited'], 
       paired = TRUE)
t.test(components$Betas[components$ROI_Location == 'Ventral_L' & components$Component_Value == 1 & components$Object_Type == 'Semantic' & components$Measurement_Type == 'Sensitivity_limited'],
       components$Betas[components$ROI_Location == 'Ventral_L' & components$Component_Value == 2 & components$Object_Type == 'Semantic' & components$Measurement_Type == 'Sensitivity_limited'], 
       paired = TRUE)
t.test(components$Betas[components$ROI_Location == 'Dorsal_R' & components$Component_Value == 1 & components$Object_Type == 'Semantic' & components$Measurement_Type == 'Sensitivity_limited'],
       components$Betas[components$ROI_Location == 'Dorsal_R' & components$Component_Value == 2 & components$Object_Type == 'Semantic' & components$Measurement_Type == 'Sensitivity_limited'], 
       paired = TRUE)
t.test(components$Betas[components$ROI_Location == 'Dorsal_L' & components$Component_Value == 1 & components$Object_Type == 'Semantic' & components$Measurement_Type == 'Sensitivity_limited'],
       components$Betas[components$ROI_Location == 'Dorsal_L' & components$Component_Value == 2 & components$Object_Type == 'Semantic' & components$Measurement_Type == 'Sensitivity_limited'], 
       paired = TRUE)
t.test(components$Betas[components$ROI_Location == 'Ventral_L' & components$Component_Value == 1 & components$Object_Type == 'Semantic' & components$Measurement_Type == 'Sensitivity_limited'],
       components$Betas[components$ROI_Location == 'Ventral_L' & components$Component_Value == 2 & components$Object_Type == 'Semantic' & components$Measurement_Type == 'Sensitivity_limited'], 
       paired = TRUE)

dorsal_L<- c(components$Betas[components$ROI_Location == 'Dorsal_L' & components$Component_Value == 1 & components$Object_Type == 'Semantic' & components$Measurement_Type == 'Sensitivity_limited'], components$Betas[components$ROI_Location == 'Dorsal_L' & components$Component_Value == 2 & components$Object_Type == 'Semantic' & components$Measurement_Type == 'Sensitivity_limited'])
dorsal_R<-  c(components$Betas[components$ROI_Location == 'Dorsal_R' & components$Component_Value == 1 & components$Object_Type == 'Semantic' & components$Measurement_Type == 'Sensitivity_limited'], components$Betas[components$ROI_Location == 'Dorsal_R' & components$Component_Value == 2 & components$Object_Type == 'Semantic' & components$Measurement_Type == 'Sensitivity_limited'])
t.test(dorsal_L, dorsal_R, paired = TRUE)

ventral_L<- c(components$Betas[components$ROI_Location == 'Ventral_L' & components$Component_Value == 1 & components$Object_Type == 'Semantic' & components$Measurement_Type == 'Sensitivity_limited'], components$Betas[components$ROI_Location == 'Ventral_L' & components$Component_Value == 2 & components$Object_Type == 'Semantic' & components$Measurement_Type == 'Sensitivity_limited'])
ventral_R<-  c(components$Betas[components$ROI_Location == 'Ventral_R' & components$Component_Value == 1 & components$Object_Type == 'Semantic' & components$Measurement_Type == 'Sensitivity_limited'], components$Betas[components$ROI_Location == 'Ventral_R' & components$Component_Value == 2 & components$Object_Type == 'Semantic' & components$Measurement_Type == 'Sensitivity_limited'])
t.test(ventral_L, ventral_R, paired = TRUE)

dorsal<- c(dorsal_L, dorsal_R)
ventral<- c(ventral_L,ventral_R)
t.test(dorsal, ventral, paired = TRUE)




t.test(components$Betas[components$ROI_Location == 'Ventral_R' & components$Component_Value == 1 & components$Object_Type == 'Novel' & components$Measurement_Type == 'Sensitivity_limited'],
       components$Betas[components$ROI_Location == 'Ventral_R' & components$Component_Value == 2 & components$Object_Type == 'Novel' & components$Measurement_Type == 'Sensitivity_limited'], 
       paired = TRUE)
t.test(components$Betas[components$ROI_Location == 'Ventral_L' & components$Component_Value == 1 & components$Object_Type == 'Novel' & components$Measurement_Type == 'Sensitivity_limited'],
       components$Betas[components$ROI_Location == 'Ventral_L' & components$Component_Value == 2 & components$Object_Type == 'Novel' & components$Measurement_Type == 'Sensitivity_limited'], 
       paired = TRUE)
t.test(components$Betas[components$ROI_Location == 'Dorsal_R' & components$Component_Value == 1 & components$Object_Type == 'Novel' & components$Measurement_Type == 'Sensitivity_limited'],
       components$Betas[components$ROI_Location == 'Dorsal_R' & components$Component_Value == 2 & components$Object_Type == 'Novel' & components$Measurement_Type == 'Sensitivity_limited'], 
       paired = TRUE)
t.test(components$Betas[components$ROI_Location == 'Dorsal_L' & components$Component_Value == 1 & components$Object_Type == 'Novel' & components$Measurement_Type == 'Sensitivity_limited'],
       components$Betas[components$ROI_Location == 'Dorsal_L' & components$Component_Value == 2 & components$Object_Type == 'Novel' & components$Measurement_Type == 'Sensitivity_limited'], 
       paired = TRUE)

t.test(semantic$Elbow_Y_loc[semantic$ROI_Location == 'ventral_right'], semantic$Elbow_Y_loc[semantic$ROI_Location == 'ventral_left'], paired = TRUE)
t.test(semantic$Elbow_Y_loc[semantic$ROI_Location == 'dorsal_right'], semantic$Elbow_Y_loc[semantic$ROI_Location == 'dorsal_left'], paired = TRUE)
t.test(semantic$Elbow_Y_loc[semantic$ROI_Location == 'dorsal_right'], semantic$Elbow_Y_loc[semantic$ROI_Location == 'ventral_right'], paired = TRUE)
t.test(semantic$Elbow_Y_loc[semantic$ROI_Location == 'dorsal_left'], semantic$Elbow_Y_loc[semantic$ROI_Location == 'ventral_left'], paired = TRUE)



t.test(novels$Elbow_Y_loc[novels$ROI_Location == 'ventral_right'], novels$Elbow_Y_loc[novels$ROI_Location == 'ventral_left'], paired = TRUE)
t.test(novels$Elbow_Y_loc[novels$ROI_Location == 'dorsal_right'], novels$Elbow_Y_loc[novels$ROI_Location == 'dorsal_left'], paired = TRUE)
t.test(novels$Elbow_Y_loc[novels$ROI_Location == 'dorsal_right'], novels$Elbow_Y_loc[novels$ROI_Location == 'ventral_right'], paired = TRUE)
t.test(novels$Elbow_Y_loc[novels$ROI_Location == 'dorsal_left'], novels$Elbow_Y_loc[novels$ROI_Location == 'ventral_left'], paired = TRUE)
