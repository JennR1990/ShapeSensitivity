OVERALLNovelA<- data.frame(IntactH = c(NA,NA,NA,NA,NA,NA,NA),S4H = c(NA,NA,NA,NA,NA,NA,NA),S16H = c(NA,NA,NA,NA,NA,NA,NA),S64H = c(NA,NA,NA,NA,NA,NA,NA),S256H = c(NA,NA,NA,NA,NA,NA,NA),IntactFA = c(NA,NA,NA,NA,NA,NA,NA),S4FA = c(NA,NA,NA,NA,NA,NA,NA),S16FA = c(NA,NA,NA,NA,NA,NA,NA),S64FA = c(NA,NA,NA,NA,NA,NA,NA),S256FA = c(NA,NA,NA,NA,NA,NA,NA), Participant = c(NA,NA,NA,NA,NA,NA,NA))
participantno<- 1
for (part in 5:17) {
  
  data<- unlist(Dataset1[,part])
  
  
  IntactH<-sum(data[Dataset1$Scramble == 1 & Dataset1$Condition == 'Same'])
  S4H<-sum(data[Dataset1$Scramble == 4 & Dataset1$Condition == 'Same'])
  S16H<-sum(data[Dataset1$Scramble == 16 & Dataset1$Condition == 'Same'])
  S64H<-sum(data[Dataset1$Scramble == 64 & Dataset1$Condition == 'Same'])
  S256H<-sum(data[Dataset1$Scramble == 256 & Dataset1$Condition == 'Same'])
  data1<- data +1
  IntactFA<-sum(data1[Dataset1$Scramble == 1 & Dataset1$Condition == 'Different'])
  S4FA<-sum(data1[Dataset1$Scramble == 4 & Dataset1$Condition == 'Different'])
  S16FA<-sum(data1[Dataset1$Scramble == 16 & Dataset1$Condition == 'Different'])
  S64FA<-sum(data1[Dataset1$Scramble == 64 & Dataset1$Condition == 'Different'])
  S256FA<-sum(data1[Dataset1$Scramble == 256 & Dataset1$Condition == 'Different'])
  
  IntactA<- ((IntactH - IntactFA)*(1-IntactFA)*100)
  
  
  Participant<-   participantno
  OVERALLNovelA[participantno,]<- c(IntactH,S4H,S16H,S64H,S256H,IntactFA,S4FA,S16FA,S64FA,S256FA, Participant)
  participantno<-   participantno + 1
}