Novel_behavioural_accuracy_per_subject <- read_csv("Novel_behavioural_accuracy_per_subject.csv")
Sem <- read_csv("Semantic_Performance.csv")




Novel_behavioural_accuracy_per_subject$Object_Type <- "Novel"
beh<- rbind(Sem, Novel_behavioural_accuracy_per_subject)

NovelPartialCorrs<- data.frame()
for (sub in 1:12){
  Correlation<- c()
  print(sub)
  filename<-sprintf("novel_beta_sub%.0f.csv", sub)
  data<- read.csv(filename, header = FALSE)
  
  
  for (ROI in 2:47){
    loc<- ROI - 1
    betas1<- as.numeric(unlist(data[1:5,ROI]))
    perf<-as.numeric(unlist(beh[beh$Participant == sub & beh$Object_Type == "Novel",1:5]))
    control<- c(1,2,3,4,5)
    model<-pcor.test(betas1, perf, control)
    Correlation[loc]<-model$estimate
  }
  Correlation[length(Correlation)+1]<- sub
  Correlation<-as.vector(Correlation)
  if (length(NovelPartialCorrs)[1]<1){
    NovelPartialCorrs<- Correlation
  }else{
    NovelPartialCorrs<- rbind(NovelPartialCorrs, Correlation)
  }
  
}
NovelPartialCorrs<- data.frame(NovelPartialCorrs)
NovelPartialCorrs$Object_Type<- 'Novel'



SemanticPartialCorrs<- data.frame()
for (sub in 1:12){
  Correlation<- c()
  print(sub)
  filename<-sprintf("semantic_beta_sub%.0f.csv", sub)
  data<- read.csv(filename, header = FALSE)
  
  
  for (ROI in 2:47){
    loc<- ROI - 1
    betas1<- as.numeric(unlist(data[1:5,ROI]))
    perf<-as.numeric(unlist(beh[beh$Participant == sub & beh$Object_Type == "Semantic",1:5]))
    control<- c(1,2,3,4,5)
    model<-pcor.test(betas1, perf, control)
    Correlation[loc]<-model$estimate
  }
  Correlation[length(Correlation)+1]<- sub
  Correlation<-as.vector(Correlation)
  if (length(SemanticPartialCorrs)[1]<1){
    SemanticPartialCorrs<- Correlation
  }else{
    SemanticPartialCorrs<- rbind(SemanticPartialCorrs, Correlation)
  }
  
}
SemanticPartialCorrs<- data.frame(SemanticPartialCorrs)

SemanticPartialCorrs$Object_Type<- 'Semantic'

ROIS<- c("lh_v1v_center", "lh_v2v_center", "lh_v3v_center", "lh_hv4", "lh_vo1", "lh_vo2", "lh_ph1", "lh_ph2", 
         "afs_l", "lh_lo1", "lh_lo2", "lh_to1", "lh_v1d_centre", "lh_v2d_centre", "lh_v3d_centre", "lh_v3a","lh_v3b",
         "lh_ips0", "lh_ips1", "lh_ips2", "lh_ips3", "lh_ips4", "lh_aIPS", "rh_v1v_center", "rh_v2v_center", 
         "rh_v3v_center", "rh_hv4", "rh_vo1", "rh_vo2", "rh_ph1", "rh_ph2", "afs_r", "rh_lo1", "rh_lo2", "rh_to1",
         "rh_v1d_centre", "rh_v2d_centre", "rh_v3d_centre", "rh_v3a","rh_v3b","rh_ips0", "rh_ips1", "rh_ips2",
         "rh_ips3", "rh_ips4", "rh_aIPS","Participant", "Object_Type")


names(SemanticPartialCorrs)<- ROIS
names(NovelPartialCorrs)<- ROIS
Correlations<- rbind(SemanticPartialCorrs, NovelPartialCorrs)




