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
