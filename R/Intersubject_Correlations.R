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

Corrs<- data.frame(Corrs)
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
intercorrs$Stream<- as.factor(intercorrs$Stream)
intercorrs$Hemisphere<- as.factor(intercorrs$Hemisphere)
intercorrs$Part<- as.factor(intercorrs$Part)
fullmodel <- ezANOVA(data=intercorrs[intercorrs$Object == "Novel",],
                     dv=Correlation,
                     wid=Subject,
                    within = c( Stream, Hemisphere, Part),
                     type=3,
                     return_aov=TRUE)
fullmodel
aovEffectSize(fullmodel$aov, effectSize = 'pes')
