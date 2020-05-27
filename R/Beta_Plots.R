Novel_behavioural_accuracy_per_subject <- read.csv("../Novel_behavioural_accuracy_per_subject.csv")
Sem <- read.csv("../Semantic_Performance.csv")
beh<- rbind(Sem, Novel_behavioural_accuracy_per_subject)
ROIS<- c("lh_v1v_center", "lh_v2v_center", "lh_v3v_center", "lh_hv4", "lh_vo1", "lh_vo2", "lh_ph1", "lh_ph2", 
         "afs_l", "lh_lo1", "lh_lo2", "lh_to1", "lh_v1d_centre", "lh_v2d_centre", "lh_v3d_centre", "lh_v3a","lh_v3b",
         "lh_ips0", "lh_ips1", "lh_ips2", "lh_ips3", "lh_ips4", "lh_aIPS", "rh_v1v_center", "rh_v2v_center", 
         "rh_v3v_center", "rh_hv4", "rh_vo1", "rh_vo2", "rh_ph1", "rh_ph2", "afs_r", "rh_lo1", "rh_lo2", "rh_to1",
         "rh_v1d_centre", "rh_v2d_centre", "rh_v3d_centre", "rh_v3a","rh_v3b","rh_ips0", "rh_ips1", "rh_ips2",
         "rh_ips3", "rh_ips4", "rh_aIPS","Participant", "Object_Type")



  for (sub in 1:13){
    R<- c()
    print(sub)
    filename<-sprintf("semantic_beta_sub%.0f.csv", sub)
    data<- read.csv(filename, header = FALSE)
    filename<-sprintf("novel_beta_sub%.0f.csv", sub)
    data1<- read.csv(filename, header = FALSE)
    newfile<- sprintf('Sub %.0f Beta Values.svg', sub)
    svglite(file=newfile, width=18, height=20, system_fonts=list(sans = "Arial"))
    layout(matrix(c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45), nrow=5, byrow=TRUE), heights=c(1,1,1,1,1))   
    for (ROI in 2:47){
      print(ROI)
      if (ROI == 10 | ROI == 33) {
        print('hi')
      } else {
      loc<- ROI - 1
      betas1<- as.numeric(unlist(data[1:5,ROI]))
      betas2<- as.numeric(unlist(data1[1:5,ROI]))
      #perf<-as.numeric(unlist(beh[beh$Participant == sub & beh$Object_Type == "Semantic",1:5]))
      control<- c(1:5)
      plot(x = control, y = betas1, main = ROIS[ROI-1], type = 'l', col = 'Blue', ylim = c(-2,3), axes = FALSE, ylab = "Beta Values", xlab = "Scrambling Level")
      axis(1, at = c(1:5), labels = c('Intact', 'S4', "S16", 'S64', 'S256'))
      axis(2, at = c(-2,-1,0,1,2,2.5), las = 2)
      lines(x = control, y = betas2, col = 'Green')
      legend(2,-1, legend = c("Semantic", "Novel"), col = c('Blue', 'Green'), lty = 1, bty = 'n')
      }
    }
    plot(x = 2, y = 0, col = 'white',axes = FALSE, ylim = c(-3.5,3), ylab = "", xlab = "")
    name<- sprintf('Subject %.0f', sub)
    text(2,0, labels = name, col = 'Black', cex = 2)
    dev.off()
  }




averagebetasperROI<- function(){
colnames<- c("lh_v1v_center", "lh_v2v_center", "lh_v3v_center", "lh_hv4", "lh_vo1", "lh_vo2", "lh_ph1", "lh_ph2", 
         "lh_lo1", "lh_lo2", "lh_to1", "lh_v1d_centre", "lh_v2d_centre", "lh_v3d_centre", "lh_v3a","lh_v3b",
         "lh_ips0", "lh_ips1", "lh_ips2", "lh_ips3", "lh_ips4", "lh_aIPS", "rh_v1v_center", "rh_v2v_center", 
         "rh_v3v_center", "rh_hv4", "rh_vo1", "rh_vo2", "rh_ph1", "rh_ph2", "rh_lo1", "rh_lo2", "rh_to1",
         "rh_v1d_centre", "rh_v2d_centre", "rh_v3d_centre", "rh_v3a","rh_v3b","rh_ips0", "rh_ips1", "rh_ips2",
         "rh_ips3", "rh_ips4", "rh_aIPS", "Scrambling","Participant")

semantic<- data.frame(one = c(NA,NA,NA,NA,NA,NA,NA),two = c(NA,NA,NA,NA,NA,NA,NA),three = c(NA,NA,NA,NA,NA,NA,NA),four = c(NA,NA,NA,NA,NA,NA,NA))
novel<- data.frame(one = c(NA,NA,NA,NA,NA,NA,NA),two = c(NA,NA,NA,NA,NA,NA,NA),three = c(NA,NA,NA,NA,NA,NA,NA),four = c(NA,NA,NA,NA,NA,NA,NA))
start<- 1
finish<- 5
for (sub in 1:12){
print(sub)
filename<-sprintf("semantic_beta_sub%.0f.csv", sub)
data<- read.csv(filename, header = FALSE)
filename<-sprintf("novel_beta_sub%.0f.csv", sub)
data1<- read.csv(filename, header = FALSE)
column<- 1

for (ROI in 2:47){

  print(ROI)
  
  if (ROI == 10 | ROI == 33) {
    print('hi')
  } else {

    
    semantic[start:finish,column]<- as.numeric(unlist(data[1:5,ROI]))
    novel[start:finish,column]<- as.numeric(unlist(data1[1:5,ROI]))

    column<- column+1



 }
}
semantic[start:finish,45]<- c("intact", "S4", "S16", "S64", "S256")
novel[start:finish,45]<- c("intact", "S4", "S16", "S64", "S256")
semantic[start:finish,46]<- sub
novel[start:finish,46]<- sub
start<- start + 5
finish<- finish + 5

}
names(semantic)<- colnames
names(novel)<- colnames








smeans1<- data.frame(Intact = c(NA,NA,NA,NA,NA,NA,NA),S4 = c(NA,NA,NA,NA,NA,NA,NA),S16 = c(NA,NA,NA,NA,NA,NA,NA),S64 = c(NA,NA,NA,NA,NA,NA,NA),S256 = c(NA,NA,NA,NA,NA,NA,NA), ROI = c(NA,NA,NA,NA,NA,NA,NA))
ses1<- data.frame(Intact = c(NA,NA,NA,NA,NA,NA,NA),S4 = c(NA,NA,NA,NA,NA,NA,NA),S16 = c(NA,NA,NA,NA,NA,NA,NA),S64 = c(NA,NA,NA,NA,NA,NA,NA),S256 = c(NA,NA,NA,NA,NA,NA,NA), ROI = c(NA,NA,NA,NA,NA,NA,NA))
nmeans1<- data.frame(Intact = c(NA,NA,NA,NA,NA,NA,NA),S4 = c(NA,NA,NA,NA,NA,NA,NA),S16 = c(NA,NA,NA,NA,NA,NA,NA),S64 = c(NA,NA,NA,NA,NA,NA,NA),S256 = c(NA,NA,NA,NA,NA,NA,NA), ROI = c(NA,NA,NA,NA,NA,NA,NA))
nes1<- data.frame(Intact = c(NA,NA,NA,NA,NA,NA,NA),S4 = c(NA,NA,NA,NA,NA,NA,NA),S16 = c(NA,NA,NA,NA,NA,NA,NA),S64 = c(NA,NA,NA,NA,NA,NA,NA),S256 = c(NA,NA,NA,NA,NA,NA,NA), ROI = c(NA,NA,NA,NA,NA,NA,NA))

level<- 1
scramlevels<-  c("intact", "S4", "S16", "S64", "S256")
for (scram in scramlevels) {

for (ROI in 1:44){
smeans1[ROI,level]<-mean(semantic[,ROI][semantic$Scrambling == scram])
ses1[ROI,level]<-(sd(semantic[,ROI][semantic$Scrambling == scram]))/ sqrt(12)
nmeans1[ROI,level]<-mean(novel[,ROI][novel$Scrambling == scram])
nes1[ROI,level]<-(sd(novel[,ROI][novel$Scrambling == scram]))/ sqrt(12)

}
  level<- level + 1
}

smeans1$ROI<- colnames[1:44]
ses1$ROI<- colnames[1:44]
nmeans1$ROI<- colnames[1:44]
nes1$ROI<- colnames[1:44]




svglite(file='Betas_Averaged.svg', width=18, height=20, system_fonts=list(sans = "Arial"))
layout(matrix(c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45), nrow=5, byrow=TRUE), heights=c(1))   
for (ROI in 1:44){
control<- c(1:5)
plot(x = control, y = smeans1[ROI,1:5], main = colnames[ROI], type = 'l', col = 'darkorchid1', ylim = c(-1,3), axes = FALSE, ylab = "Beta Values", xlab = "Scrambling Level")
arrows(x0 = control, y0= unlist(smeans1[ROI,1:5] - ses1[ROI,1:5]), x1 = control, y1 = unlist(smeans1[ROI,1:5] + ses1[ROI,1:5]), code = 3, angle = 90, length = .03, col = 'darkorchid4')
axis(1, at = c(1:5), labels = c('Intact', 'S4', "S16", 'S64', 'S256'))
axis(2, at = c(-2,-1,0,1,2,2.5), las = 2)
lines(unlist(nmeans1[ROI,1:5]), col ='deepskyblue', type = 'l' )
arrows(x0 = control, y0= unlist(nmeans1[ROI,1:5] - nes1[ROI,1:5]), x1 = control, y1 = unlist(nmeans1[ROI,1:5] + nes1[ROI,1:5]), code = 3, angle = 90, length = .03, col = 'deepskyblue3')
legend(2,-.5, legend = c("Semantic", "Novel"), col = c('darkorchid1', 'deepskyblue'), lty = 1, bty = 'n')
}

}




