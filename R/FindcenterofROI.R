ROIINFO<- function(){
  
XCENTER<- NA
XRADIUS<- NA
XLENGTH<- NA
YCENTER<- NA
YRADIUS<- NA
YLENGTH<- NA
ZCENTER<- NA
ZRADIUS<- NA
ZLENGTH<- NA
CLUSTER<- NA
#RADIUS<- NA


for (i in 1:100) {
  

  rangeX<- range(VoxelClusters$voxelX[VoxelClusters$VCluster == i])
  CenterX<- median(rangeX[1]:rangeX[2])
  RadiusX<-length(rangeX[1]:rangeX[2])/2
  rangeY<- range(VoxelClusters$voxelY[VoxelClusters$VCluster == i])
  CenterY<- median(rangeY[1]:rangeY[2])
  RadiusY<-length(rangeY[1]:rangeY[2])/2
  rangeZ<- range(VoxelClusters$voxelZ[VoxelClusters$VCluster == i])
  CenterZ<- median(rangeZ[1]:rangeZ[2])
  RadiusZ<-length(rangeZ[1]:rangeZ[2])/2
  
   XCENTER[i]<- CenterX-RadiusX
   XRADIUS[i]<- RadiusX
   XLENGTH[i]<- RadiusX*2
   YCENTER[i]<- CenterY-RadiusY
   YRADIUS[i]<- RadiusY
   YLENGTH[i]<- RadiusY*2
   ZCENTER[i]<- CenterZ-RadiusZ
   ZRADIUS[i]<- RadiusZ
   ZLENGTH[i]<- RadiusZ*2
   CLUSTER[i]<- i
   #RADIUS[i]<- min(XRADIUS[i], YRADIUS[i], ZRADIUS[i])
   
   ROIPoints<- round(data.frame(XCENTER, XRADIUS,XLENGTH, YCENTER, YRADIUS,YLENGTH, ZCENTER, ZRADIUS,ZLENGTH, CLUSTER))
   #Radius[i]<- min(ROIPoints$XRADIUS[i], ROIPoints$YRADIUS[i], ROIPoints$ZRADIUS[i])

}
return(ROIPoints)
}