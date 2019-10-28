XCENTER<- NA
XRADIUS<- NA
YCENTER<- NA
YRADIUS<- NA
ZCENTER<- NA
ZRADIUS<- NA
CLUSTER<- NA


for (i in 1:100) {
  

  rangeX<- range(clusters$X[clusters$Cluster == i])
  CenterX<- median(rangeX[1]:rangeX[2])
  RadiusX<-length(rangeX[1]:rangeX[2])/2
  rangeY<- range(clusters$Y[clusters$Cluster == i])
  CenterY<- median(rangeY[1]:rangeY[2])
  RadiusY<-length(rangeY[1]:rangeY[2])/2
  rangeZ<- range(clusters$Z[clusters$Cluster == i])
  CenterZ<- median(rangeZ[1]:rangeZ[2])
  RadiusZ<-length(rangeZ[1]:rangeZ[2])/2
  
   XCENTER[i]<- CenterX
   XRADIUS[i]<- RadiusX
   YCENTER[i]<- CenterY
   YRADIUS[i]<- RadiusY
   ZCENTER[i]<- CenterZ
   ZRADIUS[i]<- RadiusZ
   CLUSTER[i]<- i
   
   ROIPoints<- data.frame(XCENTER, XRADIUS, YCENTER, YRADIUS, ZCENTER, ZRADIUS, CLUSTER)

}
