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
  Division<-sum(c(
    IntactH<-sum(as.numeric(Response[Dataset1$Scramble == 1 & Dataset1$Answer == 1 & Response == 1])),
    S4H<-sum(as.numeric(Response[Dataset1$Scramble == 4 & Dataset1$Answer == 1 & Response == 1])),
    S16H<-sum(as.numeric(Response[Dataset1$Scramble == 16 & Dataset1$Answer == 1 & Response == 1])),
    S64H<-sum(as.numeric(Response[Dataset1$Scramble == 64 & Dataset1$Answer == 1 & Response == 1])),
    S256H<-sum(as.numeric(Response[Dataset1$Scramble == 256 & Dataset1$Answer == 1 & Response == 1])),
    
    IntactMiss<-sum(as.numeric(Response[Dataset1$Scramble == 1 & Dataset1$Answer == 1 & Response == 2]))/2,
    S4Miss<-sum(as.numeric(Response[Dataset1$Scramble == 4 & Dataset1$Answer == 1 & Response == 2]))/2,
    S16Miss<-sum(as.numeric(Response[Dataset1$Scramble == 16 & Dataset1$Answer == 1 & Response == 2]))/2,
    S64Miss<-sum(as.numeric(Response[Dataset1$Scramble == 64 & Dataset1$Answer == 1 & Response == 2]))/2,
    S256Miss<-sum(as.numeric(Response[Dataset1$Scramble == 256 & Dataset1$Answer == 1 & Response == 2]))/2))
  
  Division2<- sum(c(
    IntactH<-sum(as.numeric(Response[Dataset1$Scramble == 1 & Dataset1$Answer == 2 & Response == 1 ])),
    S4H<-sum(as.numeric(Response[Dataset1$Scramble == 4 & Dataset1$Answer == 2 & Response == 1])),
    S16H<-sum(as.numeric(Response[Dataset1$Scramble == 16 & Dataset1$Answer == 2 & Response == 1])),
    S64H<-sum(as.numeric(Response[Dataset1$Scramble == 64 & Dataset1$Answer == 2 & Response == 1])),
    S256H<-sum(as.numeric(Response[Dataset1$Scramble == 256 & Dataset1$Answer == 2 & Response == 1])),
    
    IntactMiss<-sum(as.numeric(Response[Dataset1$Scramble == 1 & Dataset1$Answer == 2 & Response == 2]))/2,
    S4Miss<-sum(as.numeric(Response[Dataset1$Scramble == 4 & Dataset1$Answer == 2 & Response == 2]))/2,
    S16Miss<-sum(as.numeric(Response[Dataset1$Scramble == 16 & Dataset1$Answer == 2 & Response == 2]))/2,
    S64Miss<-sum(as.numeric(Response[Dataset1$Scramble == 64 & Dataset1$Answer == 2 & Response == 2]))/2,
    S256Miss<-sum(as.numeric(Response[Dataset1$Scramble == 256 & Dataset1$Answer == 2 & Response == 2]))/2))
  
  
  
  IntactH<-sum(as.numeric(Response[Dataset1$Scramble == 1 & Dataset1$Answer == 1 & Response == 1]))/sum(sum(as.numeric(Response[Dataset1$Scramble == 1 & Dataset1$Answer == 1 & Response == 1])),(sum(as.numeric(Response[Dataset1$Scramble == 1 & Dataset1$Answer == 1 & Response == 2]))/2))
  S4H<-sum(as.numeric(Response[Dataset1$Scramble == 4 & Dataset1$Answer == 1 & Response == 1]))/sum(sum(as.numeric(Response[Dataset1$Scramble == 4 & Dataset1$Answer == 1 & Response == 1])),(sum(as.numeric(Response[Dataset1$Scramble == 4 & Dataset1$Answer == 1 & Response == 2]))/2))
  S16H<-sum(as.numeric(Response[Dataset1$Scramble == 16 & Dataset1$Answer == 1 & Response == 1]))/sum(sum(as.numeric(Response[Dataset1$Scramble == 16 & Dataset1$Answer == 1 & Response == 1])),(sum(as.numeric(Response[Dataset1$Scramble == 16 & Dataset1$Answer == 1 & Response == 2]))/2))
  S64H<-sum(as.numeric(Response[Dataset1$Scramble == 64 & Dataset1$Answer == 1 & Response == 1]))/sum(sum(as.numeric(Response[Dataset1$Scramble == 64 & Dataset1$Answer == 1 & Response == 1])),(sum(as.numeric(Response[Dataset1$Scramble == 64 & Dataset1$Answer == 1 & Response == 2]))/2))
  S256H<-sum(as.numeric(Response[Dataset1$Scramble == 256 & Dataset1$Answer == 1 & Response == 1]))/sum(sum(as.numeric(Response[Dataset1$Scramble == 256 & Dataset1$Answer == 1 & Response == 1])),(sum(as.numeric(Response[Dataset1$Scramble == 256 & Dataset1$Answer == 1 & Response == 2]))/2))
  
  IntactFA<-sum(as.numeric(Response[Dataset1$Scramble == 1 & Dataset1$Answer == 2 & Response == 1]))/sum(sum(as.numeric(Response[Dataset1$Scramble == 1 & Dataset1$Answer == 2 & Response == 1])),sum(as.numeric(Response[Dataset1$Scramble == 1 & Dataset1$Answer == 2 & Response == 2]))/2)
  S4FA<-sum(as.numeric(Response[Dataset1$Scramble == 4 & Dataset1$Answer == 2 & Response == 1]))/sum(sum(as.numeric(Response[Dataset1$Scramble == 4 & Dataset1$Answer == 2 & Response == 1])),sum(as.numeric(Response[Dataset1$Scramble == 4 & Dataset1$Answer == 2 & Response == 2]))/2)
  S16FA<-sum(as.numeric(Response[Dataset1$Scramble == 16 & Dataset1$Answer == 2 & Response == 1]))/sum(sum(as.numeric(Response[Dataset1$Scramble == 16 & Dataset1$Answer == 2 & Response == 1])),sum(as.numeric(Response[Dataset1$Scramble == 16 & Dataset1$Answer == 2 & Response == 2]))/2)
  S64FA<-sum(as.numeric(Response[Dataset1$Scramble == 64 & Dataset1$Answer == 2 & Response == 1]))/sum(sum(as.numeric(Response[Dataset1$Scramble == 64 & Dataset1$Answer == 2 & Response == 1])),sum(as.numeric(Response[Dataset1$Scramble == 64 & Dataset1$Answer == 2 & Response == 2]))/2)
  S256FA<-sum(as.numeric(Response[Dataset1$Scramble == 256 & Dataset1$Answer == 2 & Response == 1]))/sum(sum(as.numeric(Response[Dataset1$Scramble == 256 & Dataset1$Answer == 2 & Response == 1])),sum(as.numeric(Response[Dataset1$Scramble == 256 & Dataset1$Answer == 2 & Response == 2]))/2)
  
  
  IntactA<- (((IntactH - IntactFA)*(1-IntactFA))*100)
  S4A<- (((S4H - S4FA)*(1-S4FA))*100)
  S16A<- (((S16H - S16FA)*(1-S16FA))*100)
  S64A<- (((S64H - S64FA)*(1-S64FA))*100)
  S256A<- (((S256H - S256FA)*(1-S256FA))*100)
  
  
  
  Participant<-   participantno
  OVERALLNovelA[participantno,]<- c(IntactA,S4A,S16A,S64A,S256A, Participant)
  participantno<-   participantno + 1
}



# do \n to make a label two lines

OVERALLNovelA[14,]<- colMeans(OVERALLNovelA[1:12,])
dots<- unlist(OVERALLNovelA[1,1:5])
colors<- c('dodgerblue4', 'deepskyblue1','green4', 'springgreen','violetred4', 'violet', 'darkgoldenrod4', 'darkgoldenrod1', 'red4', 'yellow', 'darkorange2', 'tan1', 'dark blue')
plot(dots, type = 'l', axes = FALSE,main = 'Unfamiliar Objects', xlab = 'Scrambling Level', ylab = 'Accuracy', ylim = c(-5,100), col = 'white')
axis(1, at = c(1,2,3,4,5), labels= c('Intact', 'S4', 'S16', 'S64', 'S256'))
axis(2, at = c(0,20,40,60, 80, 100), labels  = c(0,20,40,60, 80, 100), las = 2)
legend(3,40, legend = c('Mean'), col = 'black', bty = 'n', lty = c(1))

abline(h = 50 ,lty = c(2), col ='grey')

for ( i in 1:13) {
  color<- colors[i]
  color<- t_col(color)
  dots<- unlist(OVERALLNovelA[i,1:5])   
  lines(dots, col = color, type = 'l')
}

dots<- unlist(OVERALLNovelA[14,1:5])   
lines(dots, col = 'black', type = 'l',lwd = 1.5)

t_col <- function(color, percent = 70, name = NULL) {
  #      color = color name
  #    percent = % transparency
  #       name = an optional name for the color
  
  ## Get RGB values for named color
  rgb.val <- col2rgb(color)
  
  ## Make new color using input color as base and alpha set by transparency
  t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3],
               max = 255,
               alpha = (100 - percent) * 255 / 100,
               names = name)
  
  ## Save the color
  invisible(t.col)
}
