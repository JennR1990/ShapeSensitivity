Corrs<- read_csv("Data/intersubjectCorrelationTransformed.csv", 
                 col_types = cols(DL = col_number(), DLA = col_number(), 
                                            DLP = col_number(), DR = col_number(), 
                                            DRA = col_number(), DRP = col_number(), 
                                            Object = col_factor(levels = c("Novel", 
                                         "Semantic",)), Subject = col_factor(levels = c("1", 
                                             "2", "3", "4", "5", "6", "7", 
                                            "8", "9", "10", "11", "12", "Average")), 
                                           VL = col_number(), VLA = col_number(), 
                                            VLP = col_number(), VR = col_number(), 
                                            VRA = col_number(), VRP = col_number()))
Corrs<-intersubjectCorrelationTransformed
Corrs<- data.frame(Corrs)
Corrs1<- c()
Corrs1$V<- c(Corrs$VR[1:12], Corrs$VL[1:12])
Corrs[25,]<- c(as.numeric(unlist(colMeans(Corrs[Corrs$Object == "Semantic",1:12]))),13,8)
Corrs[25,13]<- "Average"
Corrs[25,14]<- "Semantic"

Corrs[26,]<- c(as.numeric(unlist(colMeans(Corrs[Corrs$Object == "Novel",1:12]))),13,8)
Corrs[26,13]<- "Average"
Corrs[26,14]<- "Novel"


SemanticSE<- c()
NovelSE<- c()
for (Corr in 1:12){

  SemanticSE[Corr]<-(sd(Corrs[Corrs$Object == "Semantic",Corr]))/sqrt(12)
  NovelSE[Corr]<-(sd(Corrs[Corrs$Object == "Novel",Corr]))/sqrt(12)
  
}
svglite(file='figures/Intersubject Correlations.svg', width=12, height=8, system_fonts=list(sans = "Arial"))
layout(matrix(c(1,2,3,4,5,6), nrow=2, byrow=TRUE), heights=c(1))   
#Semantic
midpoints<-barplot(as.numeric(unlist(Corrs[25,1:4])),axes = FALSE, space = c(0,.5,.5,.5), width = .5, ylim = c(0,1.5), main = "Inter-subject Correlations", density = c(10,10,10,10),angle = c(45,45,-45,-45), col = 'darkorchid4')
axis(1, at=midpoints, labels = c('Ventral Right', 'Ventral Left', 'Dorsal Right', 'Dorsal Left'))
axis(2, at= c(0,.5,1,1.5), las = 1)
legend(0,1.4, legend = c("Semantic"), fill = c('darkorchid4'),bty = 'n')
arrows(x0 = midpoints, y0=as.numeric(unlist(Corrs[25,1:4])) - SemanticSE[1:4], x1 = midpoints, y1 = as.numeric(unlist(Corrs[25,1:4])) + SemanticSE[1:4], code = 3, angle = 90, length = .1, col = 'darkorchid4')
midpoints<-barplot(as.numeric(unlist(Corrs[25,5:8])),axes = FALSE, space = c(0,.5,.5,.5), width = .5, ylim = c(0,1.5), main = "Dorsal Inter-subject Correlations",density = c(20,20,10,10),angle = c(-45,-45,-45,-45), col = 'darkorchid4')
axis(1, at=midpoints, labels = c(' \nDorsal Left \n Posterior', ' \nDorsal Left \n Anterior', ' \nDorsal Right \n Posterior', ' \nDorsal Right \n Anterior'))
axis(2, at= c(0,.5,1,1.5), las = 1)
arrows(x0 = midpoints, y0=as.numeric(unlist(Corrs[25,5:8])) - SemanticSE[5:8], x1 = midpoints, y1 = as.numeric(unlist(Corrs[25,5:8])) + SemanticSE[5:8], code = 3, angle = 90, length = .1, col = 'darkorchid4')
midpoints<-barplot(as.numeric(unlist(Corrs[25,9:12])),axes = FALSE, space = c(0,.5,.5,.5), width = .5, ylim = c(0,1.5), main = "Ventral Inter-subject Correlations",density = c(20,20,10,10),angle = c(45,45,45,45), col = 'darkorchid4')
axis(1, at=midpoints, labels = c(' \nVentral Left \n Posterior', ' \nVentral Left \n Anterior', ' \nVentral Right \n Posterior', ' \nVentral Right \n Anterior'))
axis(2, at= c(0,.5,1,1.5), las = 1)
arrows(x0 = midpoints, y0=as.numeric(unlist(Corrs[25,9:12])) - SemanticSE[9:12], x1 = midpoints, y1 = as.numeric(unlist(Corrs[25,9:12])) + SemanticSE[9:12], code = 3, angle = 90, length = .1, col = 'darkorchid4')

#NOVEL
midpoints<-barplot(as.numeric(unlist(Corrs[26,1:4])),axes = FALSE, space = c(0,.5,.5,.5), width = .5, ylim = c(0,1.5), main = "Inter-subject Correlations", density = c(10,10,10,10),angle = c(45,45,-45,-45), col = 'deepskyblue3')
axis(1, at=midpoints, labels = c('Ventral Right', 'Ventral Left', 'Dorsal Right', 'Dorsal Left'))
axis(2, at= c(0,.5,1,1.5), las = 1)
legend(0,1.4, legend = c("Novel"), fill = c('deepskyblue3'), bty = 'n')
arrows(x0 = midpoints, y0=as.numeric(unlist(Corrs[26,1:4])) - NovelSE[1:4], x1 = midpoints, y1 = as.numeric(unlist(Corrs[26,1:4])) + NovelSE[1:4], code = 3, angle = 90, length = .1, col = 'deepskyblue3')
midpoints<-barplot(as.numeric(unlist(Corrs[26,5:8])),axes = FALSE, space = c(0,.5,.5,.5), width = .5, ylim = c(0,1.5), main = "Dorsal Inter-subject Correlations",density = c(20,20,10,10),angle = c(-45,-45,-45,-45), col = 'deepskyblue3')
axis(1, at=midpoints, labels = c(' \nDorsal Left \n Posterior', ' \nDorsal Left \n Anterior', ' \nDorsal Right \n Posterior', ' \nDorsal Right \n Anterior'))
axis(2, at= c(0,.5,1,1.5), las = 1)
arrows(x0 = midpoints, y0=as.numeric(unlist(Corrs[26,5:8])) - NovelSE[5:8], x1 = midpoints, y1 = as.numeric(unlist(Corrs[26,5:8])) + NovelSE[5:8], code = 3, angle = 90, length = .1, col = 'deepskyblue3')
midpoints<-barplot(as.numeric(unlist(Corrs[26,9:12])),axes = FALSE, space = c(0,.5,.5,.5), width = .5, ylim = c(0,1.5), main = "Ventral Inter-subject Correlations",density = c(20,20,10,10),angle = c(45,45,45,45), col = 'deepskyblue3')
axis(1, at=midpoints, labels = c(' \nVentral Left \n Posterior', ' \nVentral Left \n Anterior', ' \nVentral Right \n Posterior', ' \nVentral Right \n Anterior'))
axis(2, at= c(0,.5,1,1.5), las = 1)
arrows(x0 = midpoints, y0=as.numeric(unlist(Corrs[26,9:12])) - NovelSE[9:12], x1 = midpoints, y1 = as.numeric(unlist(Corrs[26,9:12])) + NovelSE[9:12], code = 3, angle = 90, length = .1, col = 'deepskyblue3')

dev.off()


intercorrs <- read.table("Data/intersubjectcorrelation_ANOVA.xlsx", 
                              col_types = c("text", "numeric", "text", 
                             "text", "text", "text", "text"))


intercorrs$Subject<- as.factor(intercorrs$Subject)
intercorrs$Object<- as.factor(intercorrs$Object)
intercorrs$Location<- as.factor(intercorrs$Location)
intercorrs$Hemisphere<- as.factor(intercorrs$Hemisphere)
intercorrs$Part<- as.factor(intercorrs$Part)
fullmodel <- ezANOVA(data=intercorrs,
                     dv=Correlation,
                     wid=Subject,
                    within = c( Location, Object),
                     type=3,
                     return_aov=TRUE)
fullmodel
aovEffectSize(fullmodel$aov, effectSize = 'pes')




##load no-hemi file 
Corrs<- data.frame(Corrs)
semmeans<- colMeans(Corrs[Corrs$Object == "Semantic",1:6], na.rm =TRUE)
novmeans<- colMeans(Corrs[Corrs$Object == "Novel",1:6], na.rm =TRUE)

SemanticSE<- c()
NovelSE<- c()
for (Corr in 1:6){
  
  SemanticSE[Corr]<-(sd(Corrs[Corrs$Object == "Semantic",Corr]))/sqrt(24)
  NovelSE[Corr]<-(sd(Corrs[Corrs$Object == "Novel",Corr]))/sqrt(24)
  
}
SemanticSE<- SemanticSE[3:6]
NovelSE<- NovelSE[3:6]


svglite(file='figures/Intersubject Correlations_noHemi.svg', width=8, height=8, system_fonts=list(sans = "Arial"))
layout(matrix(c(1,2,3,4), nrow=2, byrow=TRUE), heights=c(1))   
#Semantic
midpoints<-barplot(semmeans[c(5,6)],axes = FALSE, space = c(0,.5), width = .5, ylim = c(0,1.5), main = "Familiar Ventral \nInter-subject Correlations", col = 'darkorchid4', xaxt="none", cex.main = .95)
mtext('a', outer=FALSE, side=3, las=1, line=2, adj=0, padj=1, cex = 1.75)
axis(1, at=midpoints, labels = c('Posterior', 'Anterior'))
axis(2, at= c(0,.5,1,1.5), las = 1)
#legend(0,1.5, legend = c("Familiar"), fill = c('darkorchid4'),bty = 'n')
title(ylab = "Fisher Correlation")
arrows(x0 = midpoints, y0=semmeans[c(5,6)] - SemanticSE[c(5,6)], x1 = midpoints, y1 = semmeans[c(5,6)] + SemanticSE[c(5,6)], code = 3, angle = 90, length = .1, col = 'Black')
midpoints<-barplot(semmeans[c(3,4)],axes = FALSE, space = c(0,.5), width = .5, ylim = c(0,1.5), main = "Familiar Dorsal \nInter-subject Correlations", col = 'darkorchid4', xaxt="none", cex.main = .95)
mtext('b', outer=FALSE, side=3, las=1, line=2, adj=0, padj=1,cex = 1.75)
axis(1, at=midpoints, labels = c('Posterior', 'Anterior'))
axis(2, at= c(0,.5,1,1.5), las = 1)
arrows(x0 = midpoints, y0=semmeans[c(3,4)] - SemanticSE[c(3,4)], x1 = midpoints, y1 = semmeans[c(3,4)] + SemanticSE[c(3,4)], code = 3, angle = 90, length = .1, col = 'Black')
title(ylab = "Fisher Correlation")



#NOVEL

midpoints<-barplot(novmeans[c(5,6)],axes = FALSE, space = c(0,.5), width = .5, ylim = c(0,1.5), main = "Unfamiliar Ventral \nInter-subject Correlations", col = 'deepskyblue3', xaxt="none", cex.main = .95)
mtext('c', outer=FALSE, side=3, las=1, line=2, adj=0, padj=1,cex = 1.75)
axis(1, at=midpoints, labels = c('Posterior', 'Anterior'))
axis(2, at= c(0,.5,1,1.5), las = 1)
#legend(0,1.5, legend = c("Unfamiliar"), fill = c('deepskyblue3'),bty = 'n')
title(ylab = "Fisher Correlation")
arrows(x0 = midpoints, y0=novmeans[c(5,6)] - NovelSE[c(5,6)], x1 = midpoints, y1 = novmeans[c(5,6)] + NovelSE[c(5,6)], code = 3, angle = 90, length = .1, col = 'Black')
midpoints<-barplot(novmeans[c(3,4)],axes = FALSE, space = c(0,.5), width = .5, ylim = c(0,1.5), main = "Unfamiliar Dorsal \nInter-subject Correlations", col = 'deepskyblue3', xaxt="none", cex.main = .95)
mtext('d', outer=FALSE, side=3, las=1, line=2, adj=0, padj=1,cex = 1.75)
axis(1, at=midpoints, labels = c('Posterior', 'Anterior'))
axis(2, at= c(0,.5,1,1.5), las = 1)
arrows(x0 = midpoints, y0=novmeans[c(3,4)] - NovelSE[c(3,4)], x1 = midpoints, y1 = novmeans[c(3,4)] + NovelSE[c(3,4)], code = 3, angle = 90, length = .1, col = 'Black')
title(ylab = "Fisher Correlation")
dev.off()
