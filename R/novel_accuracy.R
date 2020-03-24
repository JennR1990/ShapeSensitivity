OVERALLNovel<- data.frame(Intact = c(NA,NA,NA,NA,NA,NA,NA),S4 = c(NA,NA,NA,NA,NA,NA,NA),S16 = c(NA,NA,NA,NA,NA,NA,NA),S64 = c(NA,NA,NA,NA,NA,NA,NA),S256 = c(NA,NA,NA,NA,NA,NA,NA), Participant = c(NA,NA,NA,NA,NA,NA,NA))
participantno<- 1
for (part in 5:17) {
  
  data<- unlist(Dataset1[,part])
  
  
  Intact<-sum(data[Dataset1$Scramble == 1])/80*100
  S4<-sum(data[Dataset1$Scramble == 4])/80*100
  S16<-sum(data[Dataset1$Scramble == 16])/80*100
  S64<-sum(data[Dataset1$Scramble == 64])/80*100
  S256<-sum(data[Dataset1$Scramble == 256])/80*100
  Participant<-   participantno
  OVERALLNovel[participantno,]<- c(Intact,S4,S16,S64,S256, Participant)
  participantno<-   participantno + 1
}




dots<- unlist(OVERALLNovel[1,1:5])
colors<- c('dodgerblue4', 'deepskyblue1','green4', 'springgreen','violetred4', 'violet', 'darkgoldenrod4', 'darkgoldenrod1', 'red4', 'yellow', 'darkorange2', 'tan1', 'dark blue')
plot(dots, type = 'l', axes = FALSE, xlab = 'Scrambling Level', ylab = '% Correct', ylim = c(20,100), col = 'white')
axis(1, at = c(1,2,3,4,5), labels= c('Intact', 'S4', 'S16', 'S64', 'S256'))
axis(2, at = c(20,40, 60, 80, 100), labels  = c(20,40, 60,80, 100), las = 2)
legend(3,100, legend = c('Unfamiliar'), col = 'black', bty = 'n', lty = c(1))
abline(h = 50 ,lty = c(2), col ='grey')

for ( i in 1:13) {
  color<- colors[i]
  dots<- unlist(OVERALLNovel[i,1:5])   
  lines(dots, col = color, type = 'l')
}






OVERALLNovelA<- data.frame(IntactA = c(NA,NA,NA,NA,NA,NA,NA),S4A = c(NA,NA,NA,NA,NA,NA,NA),S16A = c(NA,NA,NA,NA,NA,NA,NA),S64A =  c(NA,NA,NA,NA,NA,NA,NA),S256A = c(NA,NA,NA,NA,NA,NA,NA), Participant = c(NA,NA,NA,NA,NA,NA,NA))
participantno<- 1
for (part in 5:17) {
  
  Response<- unlist(Dataset1[,part])
  
  
  IntactH<-sum(as.numeric(Response[Dataset1$Scramble == 1 & Dataset1$Answer == 1 & Response == 1]))/80
  S4H<-sum(as.numeric(Response[Dataset1$Scramble == 4 & Dataset1$Answer == 1 & Response == 1]))/80
  S16H<-sum(as.numeric(Response[Dataset1$Scramble == 16 & Dataset1$Answer == 1 & Response == 1]))/80
  S64H<-sum(as.numeric(Response[Dataset1$Scramble == 64 & Dataset1$Answer == 1 & Response == 1]))/80
  S256H<-sum(as.numeric(Response[Dataset1$Scramble == 256 & Dataset1$Answer == 1 & Response == 1]))/80
  IntactFA<-sum(as.numeric(Response[Dataset1$Scramble == 1 & Dataset1$Answer == 2 & Response == 1]))/80
  S4FA<-sum(as.numeric(Response[Dataset1$Scramble == 4 & Dataset1$Answer == 2 & Response == 1]))/80
  S16FA<-sum(as.numeric(Response[Dataset1$Scramble == 16 & Dataset1$Answer == 2 & Response == 1]))/80
  S64FA<-sum(as.numeric(Response[Dataset1$Scramble == 64 & Dataset1$Answer == 2 & Response == 1]))/80
  S256FA<-sum(as.numeric(Response[Dataset1$Scramble == 256 & Dataset1$Answer == 2 & Response == 1]))/80
  
  IntactA<- (((IntactH - IntactFA)*(1-IntactFA))*100)
  S4A<- (((S4H - S4FA)*(1-S4FA))*100)
  S16A<- (((S16H - S16FA)*(1-S16FA))*100)
  S64A<- (((S64H - S64FA)*(1-S64FA))*100)
  S256A<- (((S256H - S256FA)*(1-S256FA))*100)
  
  
  
  Participant<-   participantno
  OVERALLNovelA[participantno,]<- c(IntactA,S4A,S16A,S64A,S256A, Participant)
  participantno<-   participantno + 1
}





dots<- unlist(OVERALLNovelA[1,1:5])
colors<- c('dodgerblue4', 'deepskyblue1','green4', 'springgreen','violetred4', 'violet', 'darkgoldenrod4', 'darkgoldenrod1', 'red4', 'yellow', 'darkorange2', 'tan1', 'dark blue')
plot(dots, type = 'l', axes = FALSE, xlab = 'Scrambling Level', ylab = '% Correct', ylim = c(-10,50), col = 'white')
axis(1, at = c(1,2,3,4,5), labels= c('Intact', 'S4', 'S16', 'S64', 'S256'))
axis(2, at = c(0,10,20,30,40, 50), labels  = c(0,10,20,30,40,50), las = 2)
legend(3,40, legend = c('Unfamiliar \n Accuracy'), col = 'black', bty = 'n', lty = c(1))
#abline(h = 50 ,lty = c(2), col ='grey')

for ( i in 1:13) {
  color<- colors[i]
  dots<- unlist(OVERALLNovelA[i,1:5])   
  lines(dots, col = color, type = 'l')
}

