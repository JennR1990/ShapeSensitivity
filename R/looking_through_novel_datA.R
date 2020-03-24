
sum(c(
IntactH<-sum(as.numeric(Response[Dataset1$Scramble == 1 & Dataset1$Answer == 1 & Response == 1])),
S4H<-sum(as.numeric(Response[Dataset1$Scramble == 4 & Dataset1$Answer == 1 & Response == 1])),
S16H<-sum(as.numeric(Response[Dataset1$Scramble == 16 & Dataset1$Answer == 1 & Response == 1])),
S64H<-sum(as.numeric(Response[Dataset1$Scramble == 64 & Dataset1$Answer == 1 & Response == 1])),
S256H<-sum(as.numeric(Response[Dataset1$Scramble == 256 & Dataset1$Answer == 1 & Response == 1])),

IntactMiss<-sum(as.numeric(Response[Dataset1$Scramble == 1 & Dataset1$Answer == 1 & Response == 2]))/2,
S4Miss<-sum(as.numeric(Response[Dataset1$Scramble == 4 & Dataset1$Answer == 1 & Response == 2]))/2,
S16Miss<-sum(as.numeric(Response[Dataset1$Scramble == 16 & Dataset1$Answer == 1 & Response == 2]))/2,
S64Miss<-sum(as.numeric(Response[Dataset1$Scramble == 64 & Dataset1$Answer == 1 & Response == 2]))/2,
S256Miss<-sum(as.numeric(Response[Dataset1$Scramble == 256 & Dataset1$Answer == 1 & Response == 2]))/2,


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


IntactFP<-sum(as.numeric(Response[Dataset1$Scramble == 1 & Dataset1$Answer == 1]))

