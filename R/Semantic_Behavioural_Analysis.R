num<-unique(Answers$Holly)


ImageNo<- c(1:80)
ImageName<- c('ac unit','flower', 'tv router', 'bag', 'barrel', 'battery', 'birds nest', 'belt', 'scale', 'shell', 'googles', 'shoe', 'bench', 
              'speaker', 'speaker', 'kids toy', 'basket', 'teddy bear', 'bag', 'sunglasses', 'table', 'tape', 'tent', 'twine', 'kleenex',
              'bow tie', 'train', 'tupperwear', 'toque', 'vhs', 'gift box', 'vhs', 'volleyball', 'washing machine', 'watch', 'watermelon', 'box', 'bucket',
              'button', 'candelabra', 'candle', 'car', 'mirror', 'champagne', 'box', 'clock', 'coffee beans', 'change', 'cooler', 'couch', 'desk',
              'cd holder', 'cables','faucet', 'feather', 'flipflop', 'floppy disk','stool', 'presents', 'hard hat', 'hat','shoe', 'shoe', 'jar', 'jeans',
              'tractor', 'lemon', 'basketball net', 'leather desk chair', 'clock', 'outlet', 'soother', 'peanuts', 'bottle', 'pinecone', 'flower pot',
              'mailbox', 'printer', 'cone', 'boombox')
CorrectAnswers<-data.frame(ImageNo, ImageName)
Image_ID<- paste(Data$IMAGE_Num, Data$Scramble, sep = '_')
Imagesshown<- Data[,1:2]
Responses<- Data[,10:22]
Answers<-cbind(Imagesshown, Responses)



performance<- data.frame()
#performance1<- data.frame()
pnum<-1
for (p in 3:15){
for (i in 1:400) {
#performance[i,pnum]<-Answers[i,p] == CorrectAnswers$ImageName[CorrectAnswers$ImageNo == Answers[i,1]]
performance[i,pnum]<-grepl(Answers[i,p], CorrectAnswers$ImageName[CorrectAnswers$ImageNo == Answers[i,1]])

}
pnum<- pnum + 1
}


performance<- performance1

performance[,1]<-as.numeric(performance$V1)
performance[,2]<-as.numeric(performance$V2)
performance[,3]<-as.numeric(performance$V3)
performance[,4]<-as.numeric(performance$V4)
performance[,5]<-as.numeric(performance$V5)
performance[,6]<-as.numeric(performance$V6)
performance[,7]<-as.numeric(performance$V7)
performance[,8]<-as.numeric(performance$V8)
performance[,9]<-as.numeric(performance$V9)
performance[,10]<-as.numeric(performance$V10)
performance[,11]<-as.numeric(performance$V11)
performance[,12]<-as.numeric(performance$V12)
performance[,13]<-as.numeric(performance$V13)
performance[,14]<- Data$Scramble

OVERALL<- data.frame(Intact = c(NA,NA,NA,NA,NA,NA,NA),S4 = c(NA,NA,NA,NA,NA,NA,NA),S16 = c(NA,NA,NA,NA,NA,NA,NA),S64 = c(NA,NA,NA,NA,NA,NA,NA),S256 = c(NA,NA,NA,NA,NA,NA,NA), Participant = c(NA,NA,NA,NA,NA,NA,NA))
for (part in 1:13) {

data<- performance[,part]  
  
Intact<-sum(data[performance$V14 == 1])/80*100
S4<-sum(data[performance$V14 == 4])/80*100
S16<-sum(data[performance$V14 == 16])/80*100
S64<-sum(data[performance$V14 == 64])/80*100
S256<-sum(data[performance$V14 == 256])/80*100
Participant<- part
OVERALL[part,]<- c(Intact,S4,S16,S64,S256, Participant) 
}



dots<- unlist(OVERALL[1,1:5])
colors<- c('dodgerblue4', 'deepskyblue1','green4', 'springgreen','violetred4', 'violet', 'darkgoldenrod4', 'darkgoldenrod1', 'red4', 'yellow', 'darkorange2', 'tan1', 'dark blue')
plot(dots, type = 'l', axes = FALSE, xlab = 'Scrambling Level', ylab = '% Correct', ylim = c(0,100), col = 'white')
axis(1, at = c(1,2,3,4,5), labels= c('Intact', 'S4', 'S16', 'S64', 'S256'))
axis(2, at = c(0, 20, 40, 60, 80, 100), labels  = c(0, 20, 40, 60,80, 100), las = 2)
legend(3,100, legend = c('Familiar'), col = 'black', bty = 'n', lty = c(1))

for ( i in 1:13) {
  color<- colors[i]
  dots<- unlist(OVERALL[i,1:5])   
  lines(dots, col = color, type = 'l')
}






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
